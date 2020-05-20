package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.hlist.Tuple2;

import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.B;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.BB;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.NB;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.R;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static java.util.Collections.emptyIterator;

interface RedBlackTree<K, V> extends Iterable<Tuple2<K, V>> {

    boolean isEmpty();

    V get(K k, Comparator<? super K> comparator);

    Tuple2<K, V> min();

    Tuple2<K, V> max();

    RedBlackTree<K, V> reverse();

    RedBlackTree<K, V> insert(K k, V v, Comparator<? super K> comparator);

    RedBlackTree<K, V> delete(K k, Comparator<? super K> comparator);

    RedBlackTree<K, V> deleteMin();

    static <K, V> RedBlackTree<K, V> empty() {
        return Spine.b();
    }

    enum Color {
        NB, R, B, BB;

        private static final Color[] VALUES = Color.values();

        public Color redder() {
            return VALUES[((ordinal() - 1) + VALUES.length) % VALUES.length];
        }

        public Color blacker() {
            return VALUES[((ordinal() + 1)) % VALUES.length];
        }
    }

    abstract class Spine<K, V> implements RedBlackTree<K, V> {

        final Color c;

        private Spine(Color c) {
            this.c = c;
        }

        abstract Spine<K, V> blacken();

        abstract Spine<K, V> redder();

        abstract Spine<K, V> ins(K newK, V newV, Comparator<? super K> comparator);

        abstract Spine<K, V> del(K k, Comparator<? super K> comparator);

        abstract Spine<K, V> removeMin();

        abstract Spine<K, V> removeMax();

        @Override
        public abstract Spine<K, V> reverse();

        @Override
        public final RedBlackTree<K, V> insert(K k, V v, Comparator<? super K> comparator) {
            return ins(k, v, comparator).blacken();
        }

        @Override
        public final RedBlackTree<K, V> delete(K k, Comparator<? super K> comparator) {
            return del(k, comparator).blacken();
        }

        @Override
        public final RedBlackTree<K, V> deleteMin() {
            return removeMin().blacken();
        }

        @SuppressWarnings("unchecked")
        static <K, V> Spine<K, V> b() {
            return (Spine<K, V>) E.B;
        }

        @SuppressWarnings("unchecked")
        static <K, V> Spine<K, V> bb() {
            return (Spine<K, V>) E.BB;
        }

        static final class Node<K, V> extends Spine<K, V> {
            private final Spine<K, V> l;
            private final K           k;
            private final V           v;
            private final Spine<K, V> r;

            Node(Color c, Spine<K, V> l, K k, V v, Spine<K, V> r) {
                super(c);
                this.l = l;
                this.k = k;
                this.v = v;
                this.r = r;
            }

            @Override
            public Node<K, V> reverse() {
                return new Node<>(c, r.reverse(), k, v, l.reverse());
            }

            @Override
            Node<K, V> blacken() {
                return c == B ? this : new Node<>(B, l, k, v, r);
            }

            @Override
            Node<K, V> redder() {
                return new Node<>(c.redder(), l, k, v, r);
            }

            @Override
            Spine<K, V> ins(K newK, V newV, Comparator<? super K> comparator) {
                int comparison = comparator.compare(newK, k);
                return comparison == 0
                       ? new Node<>(c, l, newK, newV, r)
                       : (comparison < 0
                          ? new Node<>(c, l.ins(newK, newV, comparator), k, v, r)
                          : new Node<>(c, l, k, v, r.ins(newK, newV, comparator)))
                               .balance();
            }

            @Override
            Spine<K, V> del(K k, Comparator<? super K> comparator) {
                int comparison = comparator.compare(k, this.k);
                return comparison == 0
                       ? remove()
                       : (comparison < 0
                          ? new Node<>(c, l.del(k, comparator), this.k, v, r)
                          : new Node<>(c, l, this.k, v, r.del(k, comparator))).bubble();
            }

            @Override
            Spine<K, V> removeMin() {
                return l.isEmpty() ? remove() : new Node<>(c, l.removeMin(), k, v, r).bubble();
            }

            @Override
            Spine<K, V> removeMax() {
                return r.isEmpty() ? remove() : new Node<>(c, l, k, v, r.removeMax()).bubble();
            }

            @Override
            public boolean isEmpty() {
                return false;
            }

            @Override
            public V get(K k, Comparator<? super K> comparator) {
                return comparator.compare(k, this.k) < 0
                       ? l.get(k, comparator)
                       : comparator.compare(k, this.k) == 0
                         ? v
                         : r.get(k, comparator);
            }

            @Override
            public Tuple2<K, V> min() {
                return l.isEmpty() ? tuple(k, v) : l.min();
            }

            @Override
            public Tuple2<K, V> max() {
                return r.isEmpty() ? tuple(k, v) : r.max();
            }

            @Override
            public Iterator<Tuple2<K, V>> iterator() {
                return new Iterator<Tuple2<K, V>>() {
                    private Tuple2<K, V> next;
                    private StrictStack<Object> queuedLeavesAndNodes = strictStack(Node.this);

                    @Override
                    public boolean hasNext() {
                        while (!queuedLeavesAndNodes.isEmpty() && next == null) {
                            Object nextLeafOrNode = queuedLeavesAndNodes.iterator().next();
                            queuedLeavesAndNodes = queuedLeavesAndNodes.tail();
                            if (nextLeafOrNode instanceof Node<?, ?>) {
                                @SuppressWarnings("unchecked")
                                Node<K, V> nextNode = (Node<K, V>) nextLeafOrNode;
                                if (!nextNode.r.isEmpty())
                                    queuedLeavesAndNodes = queuedLeavesAndNodes.cons(nextNode.r);
                                queuedLeavesAndNodes = queuedLeavesAndNodes.cons(tuple(nextNode.k, nextNode.v));
                                if (!nextNode.l.isEmpty())
                                    queuedLeavesAndNodes = queuedLeavesAndNodes.cons(nextNode.l);
                            } else {
                                @SuppressWarnings("unchecked")
                                Tuple2<K, V> nextLeaf = (Tuple2<K, V>) nextLeafOrNode;
                                next = nextLeaf;
                            }
                        }

                        return next != null;
                    }

                    @Override
                    public Tuple2<K, V> next() {
                        if (!hasNext())
                            throw new NoSuchElementException();
                        Tuple2<K, V> next = this.next;
                        this.next = null;
                        return next;
                    }
                };
            }

            @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (o == null || getClass() != o.getClass()) return false;
                Node<?, ?> node = (Node<?, ?>) o;
                return Objects.equals(c, node.c) &&
                        Objects.equals(l, node.l) &&
                        Objects.equals(k, node.k) &&
                        Objects.equals(v, node.v) &&
                        Objects.equals(r, node.r);
            }

            Spine<K, V> remove() {
                if (c == R && l.isEmpty() && r.isEmpty())
                    return b();

                if (c == B) {
                    if (r.isEmpty()) {
                        if (l.isEmpty()) return bb();
                        if (l.c == R) return l.blacken();
                    } else if (l.isEmpty()) {
                        if (r.c == R) return r.blacken();
                    }
                }

                Tuple2<K, V> mx = l.max();
                return new Node<>(c, l.removeMax(), mx._1(), mx._2(), r).bubble();
            }

            Spine<K, V> bubble() {
                return l.c == BB || r.c == BB
                       ? new Node<>(c.blacker(), l.redder(), k, v, r.redder()).balance()
                       : balance();
            }

            Spine<K, V> balance() {
                if (c == B) {
                    Spine<K, V> redBalanced = redden().balance0();
                    if (redBalanced != null) return redBalanced;
                } else if (c == BB) {
                    Spine<K, V> blackBalanced = blacken().balance0();
                    if (blackBalanced != null) return blackBalanced;
                    if (!r.isEmpty()) {
                        Node<K, V> r = (Node<K, V>) this.r;
                        if (r.c == NB) {
                            if (!r.l.isEmpty() && !r.r.isEmpty()) {
                                Node<K, V> rl = (Node<K, V>) r.l;
                                Node<K, V> rr = (Node<K, V>) r.r;
                                if (rl.c == B && rr.c == B) {
                                    return new Node<>(B, new Node<>(B, l, k, v, rl.l),
                                                      rl.k,
                                                      rl.v,
                                                      new Node<>(B, rl.r, r.k, r.v, rr.redden()).balance());
                                }
                            }
                        }
                    }
                    if (!l.isEmpty()) {
                        Node<K, V> l = (Node<K, V>) this.l;
                        if (l.c == NB) {
                            if (!l.l.isEmpty() && !l.r.isEmpty()) {
                                Node<K, V> ll = (Node<K, V>) l.l;
                                Node<K, V> lr = (Node<K, V>) l.r;
                                if (ll.c == B && lr.c == B) {
                                    return new Node<>(B, new Node<>(B, ll.redden(), l.k, l.v, lr.l).balance(),
                                                      lr.k,
                                                      lr.v,
                                                      new Node<>(B, lr.r, k, v, r));
                                }
                            }
                        }
                    }
                }
                return new Node<>(c, l, k, v, r);
            }

            private Node<K, V> redden() {
                return c == R ? this : new Node<>(R, l, k, v, r);
            }

            private Spine<K, V> balance0() {
                return balance0(c, l, k, v, r);
            }

            private static <K, V> Spine<K, V> balance0(Color rootColor, Spine<K, V> left, K k, V v,
                                                       Spine<K, V> right) {
                if (!left.isEmpty()) {
                    Node<K, V> l = (Node<K, V>) left;
                    if (l.c == R) {
                        if (!l.l.isEmpty()) {
                            Node<K, V> ll = (Node<K, V>) l.l;
                            if (ll.c == R) {
                                return new Node<>(rootColor, ll.blacken(), l.k, l.v, new Node<>(B, l.r, k, v, right));
                            }
                        }
                        if (!l.r.isEmpty()) {
                            Node<K, V> lr = (Node<K, V>) l.r;
                            if (lr.c == R) {
                                return new Node<>(rootColor, new Node<>(B, l.l, l.k, l.v, lr.l),
                                                  lr.k, lr.v,
                                                  new Node<>(B, lr.r, k, v, right));
                            }
                        }
                    }
                }
                if (!right.isEmpty()) {
                    Node<K, V> r = (Node<K, V>) right;
                    if (r.c == R) {
                        if (!r.l.isEmpty()) {
                            Node<K, V> rl = (Node<K, V>) r.l;
                            if (rl.c == R) {
                                return new Node<>(rootColor, new Node<>(B, left, k, v, rl.l),
                                                  rl.k, rl.v,
                                                  new Node<>(B, rl.r, r.k, r.v, r.r));
                            }
                        }
                        if (!r.r.isEmpty()) {
                            Node<K, V> rr = (Node<K, V>) r.r;
                            if (rr.c == R) {
                                return new Node<>(rootColor, new Node<>(B, left, k, v, r.l), r.k, r.v, rr.blacken());
                            }
                        }
                    }
                }
                return null;
            }
        }

        static final class E<K, V> extends Spine<K, V> {
            static final E<?, ?> B  = new E<>(Color.B);
            static final E<?, ?> BB = new E<>(Color.BB);

            private E(Color c) {
                super(c);
            }

            @Override
            public Spine<K, V> reverse() {
                return b();
            }

            @Override
            Spine<K, V> blacken() {
                return b();
            }

            @Override
            Spine<K, V> redder() {
                return b();
            }

            @Override
            Spine<K, V> ins(K k, V v, Comparator<? super K> comparator) {
                return new Node<>(R, b(), k, v, b());
            }

            @Override
            Spine<K, V> del(K k, Comparator<? super K> comparator) {
                return b();
            }

            @Override
            Spine<K, V> removeMin() {
                return b();
            }

            @Override
            Spine<K, V> removeMax() {
                return b();
            }

            @Override
            public boolean isEmpty() {
                return true;
            }

            @Override
            public V get(K k, Comparator<? super K> comparator) {
                return null;
            }

            @Override
            public Tuple2<K, V> min() {
                return null;
            }

            @Override
            public Tuple2<K, V> max() {
                return null;
            }

            @Override
            public Iterator<Tuple2<K, V>> iterator() {
                return emptyIterator();
            }
        }
    }
}
