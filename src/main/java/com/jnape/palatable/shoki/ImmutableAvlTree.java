package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.monoid.builtin.Concat;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.CatMaybes.catMaybes;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Flatten.flatten;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Cons.cons;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static java.lang.Math.abs;
import static java.lang.Math.max;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;

/**
 * @param <A>
 */
public abstract class ImmutableAvlTree<A extends Comparable<A>> implements Sizable, AvlTree<A, ImmutableAvlTree<A>> {

    private ImmutableAvlTree() {
    }

    @Override
    public abstract Maybe<A> root();

    public abstract ImmutableAvlTree<A> insert(A a);

    public abstract ImmutableAvlTree<A> remove(A a);

    public final ImmutableAvlTree<A> concat(ImmutableAvlTree<A> other) {
        if (isEmpty())
            return other;
        if (other.isEmpty())
            return this;

        ImmutableAvlTree.Node<A> thisNode = (Node<A>) this;
        ImmutableAvlTree.Node<A> thatNode = (Node<A>) other;

        int comparison = thisNode.value.compareTo(thatNode.value);
        if (comparison == 0) {
            return new Node<>(thisNode.value, thisNode.left().concat(thatNode.left()), thisNode.right().concat(thatNode.right())).balance();
        } else if (comparison < 1) {
            return new Node<>(thisNode.value, thisNode.left(), thisNode.right().concat(thatNode)).balance();
        } else {
            return new Node<>(thatNode.value, thisNode.concat(thatNode.left()), thatNode.right()).balance();
        }
    }

    @Override
    public abstract ImmutableAvlTree<A> left();

    @Override
    public abstract ImmutableAvlTree<A> right();

    @Override
    public Iterable<ImmutableAvlTree<A>> branches() {
        return root().fmap(constantly(asList(left(), right()))).orElse(emptyList());
    }

    public abstract boolean isEmpty();

    @Override
    public abstract boolean contains(A a);

    public abstract SizeInfo.Known<Byte> height();

    @Override
    public abstract SizeInfo.Known<Integer> sizeInfo();

    public abstract Iterable<A> depthFirst();

    public abstract Iterable<A> breadthFirst();

    public static <A extends Comparable<A>> ImmutableAvlTree<A> empty() {
        return Empty.instance();
    }

    public static <A extends Comparable<A>> ImmutableAvlTree<A> singleton(A a) {
        return new Node<>(a, Empty.instance(), Empty.instance());
    }

    @SafeVarargs
    public static <A extends Comparable<A>> ImmutableAvlTree<A> of(A a, A... as) {
        return from(cons(a, asList(as)));
    }

    public static <A extends Comparable<A>> ImmutableAvlTree<A> from(Iterable<A> as) {
        return foldLeft(ImmutableAvlTree::insert, empty(), as);
    }

    private static final class Empty<A extends Comparable<A>> extends ImmutableAvlTree<A> {

        private static final SizeInfo.Known<Integer> SIZE     = known(0);
        private static final Empty                   INSTANCE = new Empty();

        @Override
        public Maybe<A> root() {
            return nothing();
        }

        @Override
        public ImmutableAvlTree<A> insert(A a) {
            return singleton(a);
        }

        @Override
        public ImmutableAvlTree<A> remove(A a) {
            return this;
        }

        @Override
        public boolean contains(A a) {
            return false;
        }

        @Override
        public ImmutableAvlTree<A> left() {
            return this;
        }

        @Override
        public ImmutableAvlTree<A> right() {
            return this;
        }

        @Override
        public SizeInfo.Known<Byte> height() {
            return known((byte) 0);
        }

        @Override
        public SizeInfo.Known<Integer> sizeInfo() {
            return SIZE;
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public Iterable<A> depthFirst() {
            return emptyList();
        }

        @Override
        public Iterable<A> breadthFirst() {
            return emptyList();
        }


        @Override
        public String toString() {
            return "Empty{}";
        }

        @SuppressWarnings("unchecked")
        private static <A extends Comparable<A>> Empty<A> instance() {
            return (Empty<A>) INSTANCE;
        }
    }

    private static final class Node<A extends Comparable<A>> extends ImmutableAvlTree<A> {

        private final A                   value;
        private final ImmutableAvlTree<A> left;
        private final ImmutableAvlTree<A> right;
        private final byte                height;
        private final int                 size;

        private Node(A value, ImmutableAvlTree<A> left, ImmutableAvlTree<A> right) {
            this.value = value;
            this.left = left;
            this.right = right;
            height = (byte) (1 + max(left.height().getSize(), right.height().getSize()));
            size = 1 + left.sizeInfo().getSize() + right.sizeInfo().getSize();
        }

        @Override
        public Maybe<A> root() {
            return just(value);
        }

        @Override
        public ImmutableAvlTree<A> insert(A a) {
            int comparison = value.compareTo(a);
            Node<A> inserted = comparison == 0
                               ? this
                               : (comparison > 0)
                                 ? new Node<>(value, left.insert(a), right)
                                 : new Node<>(value, left, right.insert(a));

            return inserted.balance();
        }

        @Override
        public ImmutableAvlTree<A> remove(A a) {
            int comparison = value.compareTo(a);
            if (comparison == 0) {
                return left.concat(right);
            }
            if (comparison > 0) {
                return new Node<>(value, left.remove(a), right).balance();
            }

            return new Node<>(value, left, right.remove(a)).balance();
        }

        @Override
        public ImmutableAvlTree<A> left() {
            return left;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public boolean contains(A a) {
            int rootComparison = a.compareTo(value);
            return rootComparison == 0 || (rootComparison < 0 ? left.contains(a) : right.contains(a));
        }

        @Override
        public ImmutableAvlTree<A> right() {
            return right;
        }

        @Override
        public SizeInfo.Known<Byte> height() {
            return known(height);
        }

        @Override
        public SizeInfo.Known<Integer> sizeInfo() {
            return known(size);
        }

        @Override
        public Iterable<A> depthFirst() {
            return cons(value, Concat.concat(() -> left().depthFirst().iterator(), () -> right().depthFirst().iterator()));
        }

        @Override
        public Iterable<A> breadthFirst() {
            return cons(value, () -> catMaybes(bfs()).iterator());
        }

        private Iterable<Maybe<A>> bfs() {
            return Concat.concat(map(ImmutableAvlTree::root, branches()),
                                 flatten(map(x -> {
                                     if (x instanceof Node)
                                         return ((Node<A>) x).bfs();
                                     return emptyList();
                                 }, branches())));
        }

        @Override
        public boolean equals(Object other) {
            if (other instanceof Node) {
                boolean sameValue = this.value.equals(((Node) other).value);
                boolean sameLeft  = this.left.equals(((Node) other).left);
                boolean sameRight = this.right.equals(((Node) other).right);

                return sameValue && sameLeft && sameRight;
            }
            return false;
        }

        @Override
        public int hashCode() {
            return 31 * (31 * value.hashCode() + left.hashCode()) + right.hashCode();
        }

        @Override
        public String toString() {
            return "Node{" +
                    "value=" + value +
                    ", left=" + left +
                    ", right=" + right +
                    ", height=" + height +
                    ", size=" + size +
                    '}';
        }

        private Node<A> balance() {
            return needsRebalancing()
                   ? rightHeavy()
                     ? rotateRight()
                     : rotateLeft()
                   : this;
        }

        private boolean needsRebalancing() {
            return abs(right().height().getSize() - left.height().getSize()) > 1;
        }

        private boolean rightHeavy() {
            return right.height().getSize() - left.height().getSize() > 0;
        }

        private boolean leftHeavy() {
            return right.height().getSize() - left.height().getSize() < 0;
        }

        private Node<A> rotateRight() {
            Node<A> rightNode = (Node<A>) this.right;
            rightNode = rightNode.leftHeavy() ? rightNode.rotateLeft() : rightNode;
            return new Node<>(rightNode.value, new Node<>(value, left, rightNode.left), rightNode.right);
        }

        private Node<A> rotateLeft() {
            Node<A> leftNode = (Node<A>) this.left;
            leftNode = leftNode.rightHeavy() ? leftNode.rotateRight() : leftNode;
            return new Node<>(leftNode.value, leftNode.left, new Node<>(value, leftNode.right, right));
        }
    }
}
