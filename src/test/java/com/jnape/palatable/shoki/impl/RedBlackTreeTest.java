package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.impl.RedBlackTree.Spine.Node;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;
import testsupport.matchers.IterableMatcher;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.B;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.BB;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.NB;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Color.R;
import static com.jnape.palatable.shoki.impl.RedBlackTree.Spine.E;
import static java.util.Arrays.asList;
import static java.util.Comparator.naturalOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.iterates;

@RunWith(Enclosed.class)
public class RedBlackTreeTest {

    public static class ColorTest {

        @Test
        public void blacker() {
            assertEquals(R, NB.blacker());
            assertEquals(B, R.blacker());
            assertEquals(BB, B.blacker());
            assertEquals(NB, BB.blacker());
        }

        @Test
        public void redder() {
            assertEquals(BB, NB.redder());
            assertEquals(NB, R.redder());
            assertEquals(R, B.redder());
            assertEquals(B, BB.redder());
        }
    }

    private static <K, V> Node<K, V> b(Node<K, V> l, K k, V v, Node<K, V> r) {
        return new Node<>(B, l, k, v, r);
    }

    private static <K, V> Node<K, V> b(Node<K, V> l, K k, V v) {
        return new Node<>(B, l, k, v, RedBlackTree.Spine.b());
    }

    private static <K, V> Node<K, V> b(K k, V v, Node<K, V> r) {
        return new Node<>(B, RedBlackTree.Spine.b(), k, v, r);
    }

    private static <K, V> Node<K, V> b(K k, V v) {
        return new Node<>(B, RedBlackTree.Spine.b(), k, v, RedBlackTree.Spine.b());
    }

    private static <K, V> Node<K, V> bb(Node<K, V> l, K k, V v, Node<K, V> r) {
        return new Node<>(BB, l, k, v, r);
    }

    private static <K, V> Node<K, V> bb(Node<K, V> l, K k, V v) {
        return new Node<>(BB, l, k, v, RedBlackTree.Spine.b());
    }

    private static <K, V> Node<K, V> bb(K k, V v, Node<K, V> r) {
        return new Node<>(BB, RedBlackTree.Spine.b(), k, v, r);
    }

    private static <K, V> Node<K, V> bb(K k, V v) {
        return new Node<>(BB, RedBlackTree.Spine.b(), k, v, RedBlackTree.Spine.b());
    }

    private static <K, V> Node<K, V> r(Node<K, V> l, K k, V v, Node<K, V> r) {
        return new Node<>(R, l, k, v, r);
    }

    private static <K, V> Node<K, V> r(Node<K, V> l, K k, V v) {
        return new Node<>(R, l, k, v, RedBlackTree.Spine.b());
    }

    private static <K, V> Node<K, V> r(K k, V v, Node<K, V> r) {
        return new Node<>(R, RedBlackTree.Spine.b(), k, v, r);
    }

    private static <K, V> Node<K, V> r(K k, V v) {
        return new Node<>(R, RedBlackTree.Spine.b(), k, v, RedBlackTree.Spine.b());
    }

    private static <K, V> Node<K, V> nb(Node<K, V> l, K k, V v, Node<K, V> r) {
        return new Node<>(NB, l, k, v, r);
    }

    private static <K, V> Node<K, V> nb(K k, V v) {
        return new Node<>(NB, RedBlackTree.Spine.b(), k, v, RedBlackTree.Spine.b());
    }

    public static class NodeTest {

        @Test
        public void blacken() {
            assertEquals(RedBlackTree.Spine.b(), RedBlackTree.Spine.bb().blacken());
            assertEquals(RedBlackTree.Spine.b(), RedBlackTree.Spine.b().blacken());

            assertEquals(b(1, 1), b(1, 1).blacken());
            assertEquals(b(1, 1), bb(1, 1).blacken());
            assertEquals(b(1, 1), r(1, 1).blacken());
            assertEquals(b(1, 1), nb(1, 1).blacken());
        }

        @Test
        public void redder() {
            assertEquals(r(b(0, 0), 1, 1, b(2, 2)),
                         b(b(0, 0), 1, 1, b(2, 2)).redder());

            assertEquals(b(b(0, 0), 1, 1, b(2, 2)),
                         bb(b(0, 0), 1, 1, b(2, 2)).redder());

            assertEquals(nb(b(0, 0), 1, 1, b(2, 2)),
                         r(b(0, 0), 1, 1, b(2, 2)).redder());
        }

        @Test
        public void ins() {
            assertEquals(b(0, 0, r(1, 1)), b(0, 0).ins(1, 1, naturalOrder()));
            assertEquals(b(r(0, 0), 1, 1), b(1, 1).ins(0, 0, naturalOrder()));
            assertEquals(b(0, 1), b(0, 0).ins(0, 1, naturalOrder()));

            assertEquals(b(r(1, 1), 2, 2, r(3, 3)), b(2, 2, r(3, 3)).ins(1, 1, naturalOrder()));
            assertEquals(b(r(1, 1), 2, 2, r(3, 3)), b(r(1, 1), 2, 2).ins(3, 3, naturalOrder()));
            assertEquals(b(r(1, 2), 2, 2), b(r(1, 1), 2, 2).ins(1, 2, naturalOrder()));
            assertEquals(b(2, 2, r(3, 4)), b(2, 2, r(3, 3)).ins(3, 4, naturalOrder()));
        }

        @Test
        public void del() {
            assertEquals(RedBlackTree.Spine.<Integer, Integer>bb(),
                         b(0, 0).del(0, naturalOrder()));
            assertEquals(b(0, 0, r(2, 2)),
                         b(r(0, 0), 1, 1, r(2, 2)).del(1, naturalOrder()));
            assertEquals(b(0, 0, r(2, 2)),
                         r(b(0, 0), 1, 1, b(2, 2)).del(1, naturalOrder()));

            assertEquals(b(r(0, 0), 1, 1),
                         r(b(0, 0), 1, 1, b(2, 2)).del(2, naturalOrder()));
            assertEquals(b(1, 1, r(2, 2)),
                         r(b(0, 0), 1, 1, b(2, 2)).del(0, naturalOrder()));
        }

        @Test
        public void removeMin() {
            assertEquals(RedBlackTree.Spine.<Integer, Integer>bb(),
                         b(0, 0).removeMin());
            assertEquals(b(1, 1, r(2, 2)),
                         b(r(0, 0), 1, 1, r(2, 2)).removeMin());
            assertEquals(b(1, 1, r(2, 2)),
                         r(b(0, 0), 1, 1, b(2, 2)).removeMin());
        }

        @Test
        public void reverse() {
            assertEquals(b(1, 1), b(1, 1).reverse());
            assertEquals(b(b(0, 0), 1, 1, b(2, 2)), b(b(2, 2), 1, 1, b(0, 0)).reverse());
            assertEquals(b(r(b(1, 1), 2, 2, b(3, 3)), 4, 4, r(b(5, 5), 6, 6, b(7, 7))),
                         b(r(b(7, 7), 6, 6, b(5, 5)), 4, 4, r(b(3, 3), 2, 2, b(1, 1))).reverse());
        }

        @Test
        public void removeMax() {
            assertEquals(RedBlackTree.Spine.<Integer, Integer>bb(),
                         b(0, 0).removeMax());
            assertEquals(b(r(0, 0), 1, 1),
                         b(r(0, 0), 1, 1, r(2, 2)).removeMax());
            assertEquals(b(r(0, 0), 1, 1),
                         r(b(0, 0), 1, 1, b(2, 2)).removeMax());
        }

        @Test
        public void isEmpty() {
            assertFalse(b(1, 1).isEmpty());
        }

        @Test
        public void get() {
            assertEquals((Integer) 0, b(0, 0).get(0, naturalOrder()));
            assertNull(b(0, 0).get(1, naturalOrder()));
            assertNull(b(0, 0).get(-1, naturalOrder()));

            assertEquals((Integer) 0, b(r(0, 0), 1, 1, r(2, 2)).get(0, naturalOrder()));
            assertEquals((Integer) 1, b(r(0, 0), 1, 1, r(2, 2)).get(1, naturalOrder()));
            assertEquals((Integer) 2, b(r(0, 0), 1, 1, r(2, 2)).get(2, naturalOrder()));
            assertNull(b(r(0, 0), 1, 1, r(2, 2)).get(3, naturalOrder()));
            assertNull(b(r(0, 0), 1, 1, r(2, 2)).get(-1, naturalOrder()));

            assertEquals((Integer) 0, r(b(0, 0), 1, 1, b(2, 2)).get(0, naturalOrder()));
            assertEquals((Integer) 1, r(b(0, 0), 1, 1, b(2, 2)).get(1, naturalOrder()));
            assertEquals((Integer) 2, r(b(0, 0), 1, 1, b(2, 2)).get(2, naturalOrder()));
            assertNull(r(b(0, 0), 1, 1, b(2, 2)).get(3, naturalOrder()));
            assertNull(r(b(0, 0), 1, 1, b(2, 2)).get(-1, naturalOrder()));
        }

        @Test
        public void min() {
            assertEquals(tuple(0, 0), b(0, 0).min());
            assertEquals(tuple(0, 0), b(r(0, 0), 1, 1, r(2, 2)).min());
            assertEquals(tuple(0, 0), r(b(0, 0), 1, 1, b(2, 2)).min());
        }

        @Test
        public void max() {
            assertEquals(tuple(0, 0), b(0, 0).max());
            assertEquals(tuple(2, 2), b(r(0, 0), 1, 1, r(2, 2)).max());
            assertEquals(tuple(2, 2), r(b(0, 0), 1, 1, b(2, 2)).max());
        }

        @Test
        public void iteration() {
            assertThat(b(r(b(0, 0), 1, 1, b(2, 2)), 3, 3, r(b(4, 4), 5, 5, b(6, 6))),
                       iterates(tuple(0, 0), tuple(1, 1), tuple(2, 2),
                                tuple(3, 3), tuple(4, 4), tuple(5, 5), tuple(6, 6)));
            assertThat(r(b(r(0, 0), 1, 1, r(2, 2)), 3, 3, b(r(4, 4), 5, 5, r(6, 6))),
                       iterates(tuple(0, 0), tuple(1, 1), tuple(2, 2),
                                tuple(3, 3), tuple(4, 4), tuple(5, 5), tuple(6, 6)));
        }

        @Test
        public void balancingBalancedTrees() {
            for (Node<Integer, Integer> balancedNode :
                    asList(b(1, 1),
                           r(1, 1),
                           b(r(1, 1), 1, 1),
                           b(1, 1, r(1, 1)),
                           b(r(1, 1), 2, 2, r(3, 3)),
                           b(b(1, 1), 2, 2, b(3, 3)),
                           r(b(1, 1), 2, 2, b(3, 3)),
                           r(b(r(0, 0), 1, 1, r(2, 2)), 3, 3, b(r(4, 4), 5, 5, r(6, 6))))) {
                assertEquals(balancedNode, balancedNode.balance());
            }
        }

        @Test
        public void balanceB() {
            assertEquals(r(b(1, 1), 2, 2, b(3, 3)),
                         b(r(r(1, 1), 2, 2), 3, 3).balance());

            assertEquals(r(b(1, 1), 2, 2, b(3, 3)),
                         b(r(1, 1, r(2, 2)), 3, 3).balance());

            assertEquals(r(b(1, 1), 2, 2, b(3, 3)),
                         b(1, 1, r(2, 2, r(3, 3))).balance());

            assertEquals(r(b(1, 1), 2, 2, b(3, 3)),
                         b(1, 1, r(r(2, 2), 3, 3)).balance());
        }

        @Test
        public void balanceBB() {
            assertEquals(b(b(1, 1), 2, 2, b(3, 3)),
                         bb(r(r(1, 1), 2, 2), 3, 3).balance());

            assertEquals(b(b(1, 1), 2, 2, b(3, 3)),
                         bb(r(1, 1, r(2, 2)), 3, 3).balance());

            assertEquals(b(b(1, 1), 2, 2, b(3, 3)),
                         bb(1, 1, r(2, 2, r(3, 3))).balance());

            assertEquals(b(b(1, 1), 2, 2, b(3, 3)),
                         bb(1, 1, r(r(2, 2), 3, 3)).balance());

            assertEquals(b(b(1, 1), 2, 2, b(3, 3)),
                         bb(1, 1, r(r(2, 2), 3, 3)).balance());

            assertEquals(b(b(r(b(0, 0), 1, 1, b(2, 2)),
                             3, 3,
                             r(b(4, 4), 5, 5, b(6, 6))),
                           7, 7,
                           b(8, 8, r(9, 9))),
                         bb(r(b(0, 0), 1, 1, b(2, 2)),
                            3, 3,
                            nb(b(r(b(4, 4), 5, 5, b(6, 6)), 7, 7),
                               8, 8,
                               b(9, 9))).balance());

            assertEquals(b(r(b(b(0, 0), 1, 1, b(2, 2)),
                             3, 3,
                             b(4, 4)),
                           5, 5,
                           b(6, 6, r(b(7, 7), 8, 8, b(9, 9)))),
                         bb(nb(b(r(b(0, 0), 1, 1, b(2, 2)), 3, 3),
                               4, 4,
                               b(5, 5)),
                            6, 6,
                            r(b(7, 7), 8, 8, b(9, 9))).balance());
        }

        @Test
        public void bubble() {
            assertEquals(new Node<>(BB, RedBlackTree.Spine.b(), 1, 1, RedBlackTree.Spine.b()),
                         new Node<>(B, RedBlackTree.Spine.bb(), 1, 1, RedBlackTree.Spine.bb()).bubble());

            assertEquals(new Node<>(BB, new Node<>(B, RedBlackTree.Spine.b(), 1, 1, RedBlackTree.Spine.b()), 2, 2, new Node<>(B, RedBlackTree.Spine.b(), 3, 3, RedBlackTree.Spine.b())),
                         new Node<>(B, new Node<>(BB, RedBlackTree.Spine.b(), 1, 1, RedBlackTree.Spine.b()), 2, 2, new Node<>(BB, RedBlackTree.Spine.b(), 3, 3, RedBlackTree.Spine.b())).bubble());

            assertEquals(new Node<>(B, RedBlackTree.Spine.b(), 1, 1, RedBlackTree.Spine.b()),
                         new Node<>(B, RedBlackTree.Spine.b(), 1, 1, RedBlackTree.Spine.b()).bubble());
        }
    }

    public static class Empty {

        @Test
        public void blacken() {
            assertEquals(E.B, E.B.blacken());
            assertEquals(E.B, E.BB.blacken());
        }

        @Test
        public void redder() {
            assertEquals(E.B, E.B.redder());
            assertEquals(E.B, E.BB.redder());
        }

        @Test
        public void reverse() {
            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>b().reverse());

            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>bb().reverse());
        }

        @Test
        public void ins() {
            assertEquals(r(1, 1), RedBlackTree.Spine.<Integer, Integer>b().ins(1, 1, naturalOrder()));
            assertEquals(r(1, 1), RedBlackTree.Spine.<Integer, Integer>bb().ins(1, 1, naturalOrder()));
        }

        @Test
        public void del() {
            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>b().del(1, naturalOrder()));
            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>bb().del(1, naturalOrder()));
        }

        @Test
        public void removeMin() {
            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>b().removeMin());
            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>bb().removeMin());
        }

        @Test
        public void removeMax() {
            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>b().removeMax());
            assertEquals(RedBlackTree.Spine.<Integer, Integer>b(),
                         RedBlackTree.Spine.<Integer, Integer>bb().removeMax());
        }

        @Test
        public void isEmpty() {
            assertTrue(E.B.isEmpty());
            assertTrue(E.BB.isEmpty());
        }

        @Test
        public void get() {
            assertNull(RedBlackTree.Spine.<Integer, Integer>b().get(0, naturalOrder()));
            assertNull(RedBlackTree.Spine.<Integer, Integer>bb().get(0, naturalOrder()));
        }

        @Test
        public void min() {
            assertNull(RedBlackTree.Spine.<Integer, Integer>b().min());
            assertNull(RedBlackTree.Spine.<Integer, Integer>bb().min());
        }

        @Test
        public void max() {
            assertNull(RedBlackTree.Spine.<Integer, Integer>b().max());
            assertNull(RedBlackTree.Spine.<Integer, Integer>bb().max());
        }

        @Test
        public void iteration() {
            assertThat(RedBlackTree.Spine.<Integer, Integer>b(), IterableMatcher.isEmpty());
            assertThat(RedBlackTree.Spine.<Integer, Integer>bb(), IterableMatcher.isEmpty());
        }
    }
}