package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.PriorityCollection;
import com.jnape.palatable.shoki.api.SizeInfo;
import com.jnape.palatable.shoki.impl.SkewBinomialHeap.Node;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.Fn0.fn0;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Cons.cons;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.functions.builtin.fn3.LTEWith.lteWith;
import static com.jnape.palatable.lambda.functions.recursion.RecursiveResult.recurse;
import static com.jnape.palatable.lambda.functions.recursion.RecursiveResult.terminate;
import static com.jnape.palatable.lambda.functions.recursion.Trampoline.trampoline;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.sameElementsSameOrder;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.SkewBinomialHeap.deleteMin;
import static com.jnape.palatable.shoki.impl.SkewBinomialHeap.findMin;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;

/**
 * A <a href="https://www.brics.dk/RS/96/37/BRICS-RS-96-37.pdf" target="_new">bootstrapped skew binomial queue</a>
 * implementation of a {@link PriorityCollection} offering worst-case <code>O(1)</code>
 * {@link PriorityHeap#insert(Object) insertion} and {@link PriorityHeap#head() min lookup} operations and an
 * <code>O(log n)</code> {@link PriorityHeap#tail() min deletion} operation.
 * <p>
 * A <a href="https://en.wikipedia.org/wiki/Heap_(data_structure)" target="_new">heap</a> is a tree-based data
 * structure that maintains an ordering constraint between nodes of different levels. For instance, a "min heap" is a
 * heap with an ordering constraint that ensures an ascending ordering of elements from root to leaf: that is, all
 * nodes below the current node in the branch are less than or equal to the current node (a "max heap" would merely
 * invert the ordering constraint). A min heap storing the natural numbers <code>1..5</code> as integers might be
 * visually represented as:
 * <pre>
 *      1
 *     / \
 *    /   \
 *   2     5
 *  / \
 * 3   4
 * </pre>
 * A <a href="https://en.wikipedia.org/wiki/Binomial_heap" target="_top">binomial heap</a> (or binomial queue) is a
 * heap that is implemented in terms of a binomial tree: a tree structure inspired by binary numerical representation
 * with a root node and a rank <code>k</code>, and whose child nodes are arranged as binomial subtrees, ordered from
 * left to right by descending ranks. The term "binomial tree" is used because a binomial tree of rank <code>k</code>
 * has <code>(k; d)</code> - <em>"k choose d"</em> - nodes at a depth <code>d</code>. Binomial heaps tend to offer
 * <code>O(log n)</code> insert, min lookup, merge, and min deletion operations. Binomial trees of ranks
 * <code>0..3</code> can be visualized as follows:
 * <pre>
 * Rank 0    Rank 1      Rank 2        Rank 3
 *   ○         ○           ○             ○
 *             |          /|            /|\
 *             ○         ○ ○           ○ ○ ○
 *                       |            /| |
 *                       ○           ○ ○ ○
 *                                   |
 *                                   ○
 * </pre>
 * <a href="https://en.wikipedia.org/wiki/Skew_binomial_heap" target="_top">Skew binomial queues</a> improve upon
 * binomial heaps by offering a worst-case <code>O(1)</code> time complexity insertion operation via a forest
 * representation of child binomial trees, allowing new trees to be added to the forest with a constant time "skew
 * link" at no additional cost to any of the other logarithmic operations. Finally, the addition of a global root
 * maintaining the current minimum of all the forests of a skew binomial queue improves both min lookup and merge to
 * worst-case <code>O(1)</code>.
 * <p>
 * {@link PriorityHeap} is therefore implemented as a skew binomial queue with a global root, supporting a configurable
 * ordering constraint via a custom {@link Comparator} - allowing type-safe storage of elements that need not
 * themselves be {@link Comparable} - and default configurations for common
 * {@link PriorityHeap#min(Comparable[]) min-heap}/{@link PriorityHeap#max(Comparable[]) max-heap} variants.
 *
 * @param <A> the element type
 */
public abstract class PriorityHeap<A> implements PriorityCollection<Natural, A> {

    protected final Comparator<? super A> comparator;

    private PriorityHeap(Comparator<? super A> comparator) {
        this.comparator = comparator;
    }

    /**
     * {@inheritDoc}
     * <code>O(log n)</code>.
     */
    @Override
    public abstract PriorityHeap<A> tail();

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public abstract PriorityHeap<A> insert(A a);

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public final PriorityHeap<A> prioritize(Comparator<? super A> comparator) {
        return Objects.equals(this.comparator, comparator) ? this : priorityHeap(comparator, this);
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public final PriorityHeap<A> reverse() {
        return prioritize(comparator.reversed());
    }

    public abstract Iterable<A> fastUnordered();

    @Override
    public final boolean equals(Object other) {
        EquivalenceRelation<PriorityHeap<A>> sameComparators = (x, y) -> equivalent(x.comparator, y.comparator, objectEquals());
        return (other instanceof PriorityHeap<?>) &&
                equivalent(this, downcast(other),
                           sameComparators.and(sameElementsSameOrder(objectEquals())));
    }

    @Override
    public final int hashCode() {
        return HashingAlgorithms.<A, PriorityHeap<A>>elementsInOrder(objectHashCode()).apply(this);
    }

    @Override
    public String toString() {
        return "Heap[" + String.join(" ∘ ", map(Object::toString, this)) + "]";
    }

    public static <A> PriorityHeap<A> empty(Comparator<? super A> comparator) {
        return priorityHeap(comparator, emptyList());
    }

    @SafeVarargs
    public static <A> PriorityHeap<A> of(Comparator<? super A> comparator, A a, A... as) {
        return priorityHeap(comparator, cons(a, asList(as)));
    }

    @SafeVarargs
    public static <A extends Comparable<? super A>> PriorityHeap<A> min(A... as) {
        return priorityHeap(naturalOrder(), asList(as));
    }

    @SafeVarargs
    public static <A extends Comparable<? super A>> PriorityHeap<A> max(A... as) {
        return priorityHeap(reverseOrder(), asList(as));
    }

    private static <A> PriorityHeap<A> priorityHeap(Comparator<? super A> comparator, Iterable<A> as) {
        return foldLeft(PriorityHeap::insert, (PriorityHeap<A>) new Empty<A>(comparator), as);
    }

    private static final class Empty<A> extends PriorityHeap<A> {
        private Empty(Comparator<? super A> comparator) {
            super(comparator);
        }

        @Override
        public PriorityHeap<A> tail() {
            return this;
        }

        @Override
        public PriorityHeap<A> insert(A a) {
            return new NonEmpty<>(comparator, new Node<>(a, 0, StrictStack.empty()));
        }

        @Override
        public SizeInfo.Known<Natural> sizeInfo() {
            return known(zero());
        }

        @Override
        public Maybe<A> head() {
            return nothing();
        }

        @Override
        public Iterable<A> fastUnordered() {
            return Collections::emptyIterator;
        }
    }

    private static final class NonEmpty<A> extends PriorityHeap<A> {
        private final Node<A> root;

        private NonEmpty(Comparator<? super A> comparator, Node<A> root) {
            super(comparator);
            this.root = root;
        }

        @Override
        public PriorityHeap<A> tail() {

            return findMin(root.children, comparator)
                    .match(fn0(() -> empty(comparator)),
                           minChild -> new NonEmpty<>(comparator,
                                                      new Node<>(minChild, 0,
                                                                 deleteMin(root.children, comparator))));
        }

        @Override
        public PriorityHeap<A> insert(A a) {
            Node<A> inserted = lteWith(comparator, root.value, a)
                               ? new Node<>(a, 0, SkewBinomialHeap.insert(root.value, root.children, comparator))
                               : new Node<>(root.value, 0, SkewBinomialHeap.insert(a, root.children, comparator));
            return new NonEmpty<>(comparator, inserted);
        }

        @Override
        public SizeInfo.Known<Natural> sizeInfo() {
            return known(abs((long) trampoline(into((sum, rest) -> rest.head().match(
                    fn0(() -> terminate(sum)),
                    next -> recurse(tuple(sum + 1, rest.tail().consAll(next.children))))), tuple(1, root.children))));
        }

        @Override
        public Maybe<A> head() {
            return just(root.value);
        }

        @Override
        public Iterable<A> fastUnordered() {
            return () -> new Iterator<A>() {
                StrictStack<Node<A>> queued = StrictStack.of(root);

                @Override
                public boolean hasNext() {
                    return !queued.isEmpty();
                }

                @Override
                public A next() {
                    Node<A> next = queued.head().orElseThrow(NoSuchElementException::new);
                    queued = queued.tail().consAll(next.children);
                    return next.value;
                }
            };
        }
    }

}
