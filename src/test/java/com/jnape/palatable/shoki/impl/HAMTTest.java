package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.hlist.HList;
import com.jnape.palatable.shoki.impl.HAMT.Collision;
import com.jnape.palatable.shoki.impl.HAMT.Entry;
import com.jnape.palatable.shoki.impl.HAMT.Node;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.referenceEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.impl.Bitmap32.populateAtIndex;
import static com.jnape.palatable.shoki.impl.HAMT.MAX_LEVEL;
import static com.jnape.palatable.shoki.impl.HAMT.Node.rootNode;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static testsupport.matchers.IterableMatcher.iterates;

@RunWith(Enclosed.class)
public class HAMTTest {

    public static final class EntryTest {

        private Entry<String, Integer> entry;

        @Before
        public void setUp() {
            entry = new Entry<>("foo", 1);
        }

        @Test
        public void product() {
            assertEquals(tuple("foo", 1), entry.into(HList::tuple));
        }

        @Test
        public void iteratesKeyValue() {
            assertThat(entry, iterates(tuple("foo", 1)));
        }

        @Test
        public void get() {
            assertEquals(just(1), entry.get("foo", 0, objectEquals(), -1));
            assertEquals(nothing(), entry.get("bar", 0, objectEquals(), -1));
            assertEquals(nothing(), entry.get("foo", 0, (x, y) -> false, -1));
        }

        @Test
        public void remove() {
            assertEquals(nothing(), entry.remove("foo", 0, objectEquals(), -1));
            assertEquals(just(entry), entry.remove("bar", 0, objectEquals(), -1));
            assertEquals(just(entry), entry.remove("foo", 0, (x, y) -> false, -1));
        }

        @Test
        public void put() {
            assertEquals(new Entry<>("foo", 2),
                         entry.put("foo", 2, "foo".hashCode(), objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(populateAtIndex(populateAtIndex(0, "foo".hashCode() & 31),
                                                    "bar".hashCode() & 31),
                                    new Object[]{new Entry<>("foo", 1), new Entry<>("bar", 2)}),
                         entry.put("bar", 2, "bar".hashCode(), objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(1,
                                    new Object[]{new Collision<>("foo".hashCode(),
                                                                 StrictStack.of(new Entry<>("foo", 1),
                                                                                new Entry<>("bar", 2)))}),
                         entry.put("bar", 2, 0, objectEquals(), objectHashCode(), MAX_LEVEL));
        }
    }

    public static final class CollisionTest {

        private Collision<String, Integer> collision;

        @Before
        public void setUp() {
            collision = new Collision<>(0, StrictStack.of(new Entry<>("baz", 3),
                                                          new Entry<>("bar", 2),
                                                          new Entry<>("foo", 1)));
        }

        @Test
        public void iteratesEntries() {
            assertThat(collision,
                       iterates(tuple("foo", 1),
                                tuple("bar", 2),
                                tuple("baz", 3)));
        }

        @Test
        public void get() {
            assertEquals(just(1), collision.get("foo", 0, objectEquals(), -1));
            assertEquals(just(2), collision.get("bar", 0, objectEquals(), -1));
            assertEquals(just(3), collision.get("baz", 0, objectEquals(), -1));
            assertEquals(nothing(), collision.get("foo", 1, objectEquals(), -1));
            assertEquals(nothing(), collision.get("foo", 0, (x, y) -> false, -1));
        }

        @Test
        public void put() {
            assertEquals(new Collision<>(0, StrictStack.of(new Entry<>("foo", -1),
                                                           new Entry<>("bar", 2),
                                                           new Entry<>("baz", 3))),
                         collision.put("foo", -1, 0, objectEquals(), objectHashCode(), -1));

            assertEquals(new Collision<>(0, StrictStack.of(new Entry<>("qux", 0),
                                                           new Entry<>("foo", 1),
                                                           new Entry<>("bar", 2),
                                                           new Entry<>("baz", 3))),
                         collision.put("qux", 0, 0, objectEquals(), objectHashCode(), -1));

            assertEquals(new Collision<>(0, StrictStack.of(new Entry<>("foo", 0),
                                                           new Entry<>("foo", 1),
                                                           new Entry<>("bar", 2),
                                                           new Entry<>("baz", 3))),
                         collision.put("foo", 0, 0, (x, y) -> false, objectHashCode(), -1));
        }

        @Test
        public void remove() {
            assertEquals(just(new Collision<>(0, StrictStack.of(new Entry<>("bar", 2),
                                                                new Entry<>("baz", 3)))),
                         collision.remove("foo", 0, objectEquals(), 1));
            assertEquals(just(new Collision<>(0, StrictStack.of(new Entry<>("foo", 1),
                                                                new Entry<>("baz", 3)))),
                         collision.remove("bar", 0, objectEquals(), 1));
            assertEquals(just(new Collision<>(0, StrictStack.of(new Entry<>("foo", 1),
                                                                new Entry<>("bar", 2)))),
                         collision.remove("baz", 0, objectEquals(), 1));

            assertEquals(just(new Collision<>(0, StrictStack.of(new Entry<>("foo", 1),
                                                                new Entry<>("bar", 2),
                                                                new Entry<>("baz", 3)))),
                         collision.remove("missing", 0, objectEquals(), 1));

            assertEquals(just(new Collision<>(0, StrictStack.of(new Entry<>("foo", 1),
                                                                new Entry<>("bar", 2),
                                                                new Entry<>("baz", 3)))),
                         collision.remove("foo", 0, (x, y) -> false, 1));

            assertEquals(just(collision), collision.remove("foo", -1, objectEquals(), 1));

            assertEquals(just(new Entry<>("baz", 3)),
                         collision
                                 .remove("foo", 0, objectEquals(), 1)
                                 .flatMap(c -> c.remove("bar", 0, objectEquals(), 1)));

            assertEquals(just(new Entry<>("baz", 3)),
                         collision
                                 .remove("foo", 0, objectEquals(), 1)
                                 .flatMap(c -> c.remove("bar", 0, objectEquals(), 1)));
        }

        @Test
        public void equalsAndHashCode() {
            assertEquals(new Collision<>(0, StrictStack.empty()), new Collision<>(0, StrictStack.empty()));
            assertNotEquals(new Collision<>(0, StrictStack.empty()),
                            new Collision<>(1, StrictStack.empty()));
            assertEquals(new Collision<>(0, StrictStack.of(new Entry<>("foo", 1))),
                         new Collision<>(0, StrictStack.of(new Entry<>("foo", 1))));
            assertNotEquals(new Collision<>(0, StrictStack.empty()),
                            new Collision<>(0, StrictStack.of(new Entry<>("foo", 1))));

            assertNotEquals(new Collision<>(0, StrictStack.empty()), new Object());
        }
    }

    public static final class NodeTest {

        private static final Integer foo  = 0b00_00000_00000_00000_00000_00000_00000;
        private static final Integer bar  = 0b00_00000_00000_00000_00000_00000_00001;
        private static final Integer baz  = 0b00_00000_00000_00000_10000_00001_00001;
        private static final Integer quux = 0b00_00000_00000_00000_10000_00001_00001;

        @Test
        public void iteratesAllEntries() {
            assertThat(new Node<>(0, new Object[]{
                               new Entry<>("foo", 1),
                               new Node<>(0, new Object[]{new Entry<>("bar", 2)}),
                               new Collision<>(0, StrictStack.of(new Entry<>("quux", 4),
                                                                 new Entry<>("baz", 3)))}),
                       iterates(tuple("foo", 1),
                                tuple("bar", 2),
                                tuple("baz", 3),
                                tuple("quux", 4)));
        }

        @Test
        public void rootNodeSingleton() {
            assertSame(rootNode(), rootNode());
        }

        @Test
        public void get() {
            Node<Integer, String> node = new Node<Integer, String>(0, new Object[0])
                    .put(foo, "foo", foo, referenceEquals(), objectHashCode(), 1)
                    .put(bar, "bar", bar, referenceEquals(), objectHashCode(), 1)
                    .put(baz, "baz", baz, referenceEquals(), objectHashCode(), 1)
                    .put(quux, "quux", quux, referenceEquals(), objectHashCode(), 1);

            assertEquals(just("foo"), node.get(foo, foo, referenceEquals(), 1));
            assertEquals(just("bar"), node.get(bar, bar, referenceEquals(), 1));
            assertEquals(just("baz"), node.get(baz, baz, referenceEquals(), 1));
            assertEquals(just("quux"), node.get(quux, quux, referenceEquals(), 1));

            assertEquals(nothing(), node.get(-1, foo, referenceEquals(), 1));
            assertEquals(nothing(), node.get(foo, -1, referenceEquals(), 1));
        }

        @Test
        public void put() {
            assertEquals(new Node<>(1, new Object[]{new Entry<>(foo, "foo")}),
                         rootNode().put(foo, "foo", foo, objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(2, new Object[]{new Entry<>(bar, "bar")}),
                         rootNode().put(bar, "bar", bar, objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(3, new Object[]{new Entry<>(foo, "foo"), new Entry<>(bar, "bar")}),
                         rootNode()
                                 .put(foo, "foo", foo, objectEquals(), objectHashCode(), 1)
                                 .put(bar, "bar", bar, objectEquals(), objectHashCode(), 1));

            assertEquals(new Node<>(3, new Object[]{
                                 new Entry<>(foo, "foo"),
                                 new Node<>(3, new Object[]{
                                         new Entry<>(bar, "bar"),
                                         new Entry<>(baz, "baz")
                                 })}),
                         rootNode()
                                 .put(foo, "foo", foo, objectEquals(), objectHashCode(), 1)
                                 .put(bar, "bar", bar, objectEquals(), objectHashCode(), 1)
                                 .put(baz, "baz", baz, objectEquals(), objectHashCode(), 1));

            assertEquals(
                    new Node<>(3, new Object[]{
                            new Entry<>(foo, "foo"),
                            new Node<>(3, new Object[]{
                                    new Entry<>(bar, "bar"),
                                    new Node<>(0b00_00000_00000_00010_00000_00000_00000, new Object[]{
                                            new Node<>(1, new Object[]{
                                                    new Node<>(1, new Object[]{
                                                            new Node<>(1, new Object[]{
                                                                    new Node<>(1, new Object[]{
                                                                            new Collision<>(
                                                                                    0b10000_00001_00001,
                                                                                    StrictStack.of(
                                                                                            new Entry<>(baz, "baz"),
                                                                                            new Entry<>(quux, "quux")))
                                                                    })
                                                            })
                                                    })
                                            })
                                    })
                            })}),
                    rootNode()
                            .put(foo, "foo", foo, referenceEquals(), objectHashCode(), 1)
                            .put(bar, "bar", bar, referenceEquals(), objectHashCode(), 1)
                            .put(baz, "baz", baz, referenceEquals(), objectHashCode(), 1)
                            .put(quux, "quux", quux, referenceEquals(), objectHashCode(), 1));
        }

        @Test
        public void remove() {
            assertEquals(just(Node.<Integer, String>rootNode()),
                         Node.<Integer, String>rootNode().remove(-1, -1, objectEquals(), -1));

            assertEquals(just(Node.<Integer, String>rootNode()),
                         Node.<Integer, String>rootNode()
                                 .put(foo, "foo", foo, objectEquals(), objectHashCode(), 1)
                                 .remove(foo, foo, objectEquals(), 1));

            assertEquals(just(Node.<Integer, String>rootNode()
                                      .put(foo, "foo", foo, objectEquals(), objectHashCode(), 1)),
                         Node.<Integer, String>rootNode()
                                 .put(foo, "foo", foo, objectEquals(), objectHashCode(), 1)
                                 .remove(foo, -1, objectEquals(), 1));

            assertEquals(just(Node.<Integer, String>rootNode()
                                      .put(bar, "bar", bar, objectEquals(), objectHashCode(), 1)),
                         Node.<Integer, String>rootNode()
                                 .put(foo, "foo", foo, objectEquals(), objectHashCode(), 1)
                                 .put(bar, "bar", bar, objectEquals(), objectHashCode(), 1)
                                 .remove(foo, foo, objectEquals(), 1));

            assertEquals(just(new Node<>(2, new Object[]{
                                 new Node<>(2, new Object[]{
                                         new Node<>(0b10_00000_00000_00000, new Object[]{
                                                 new Node<>(1, new Object[]{
                                                         new Node<>(1, new Object[]{
                                                                 new Node<>(1, new Object[]{
                                                                         new Node<>(1, new Object[]{
                                                                                 new Entry<>(baz, "baz")
                                                                         })
                                                                 })
                                                         })
                                                 })
                                         })
                                 })
                         })),
                         Node.<Integer, String>rootNode()
                                 .put(baz, "baz", baz, referenceEquals(), objectHashCode(), 1)
                                 .put(quux, "quux", quux, referenceEquals(), objectHashCode(), 1)
                                 .remove(quux, quux, referenceEquals(), 1));
        }
    }
}