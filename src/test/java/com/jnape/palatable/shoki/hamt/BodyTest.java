package com.jnape.palatable.shoki.hamt;

import com.jnape.palatable.lambda.adt.hlist.HList;
import com.jnape.palatable.shoki.ImmutableStack;
import com.jnape.palatable.shoki.hamt.Body.Collision;
import com.jnape.palatable.shoki.hamt.Body.Entry;
import com.jnape.palatable.shoki.hamt.Body.Node;
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
import static com.jnape.palatable.shoki.hamt.Bitmap32.bitmap32;
import static com.jnape.palatable.shoki.hamt.Bitmap32.empty;
import static com.jnape.palatable.shoki.hamt.Body.MAX_LEVEL;
import static com.jnape.palatable.shoki.hamt.Body.Node.rootNode;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static testsupport.matchers.IterableMatcher.iterates;

@RunWith(Enclosed.class)
public class BodyTest {

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
            assertEquals(just(1), entry.get("foo", Bitmap32.empty(), objectEquals(), -1));
            assertEquals(nothing(), entry.get("bar", Bitmap32.empty(), objectEquals(), -1));
            assertEquals(nothing(), entry.get("foo", Bitmap32.empty(), (x, y) -> false, -1));
        }

        @Test
        public void remove() {
            assertEquals(nothing(), entry.remove("foo", Bitmap32.empty(), objectEquals(), -1));
            assertEquals(just(entry), entry.remove("bar", Bitmap32.empty(), objectEquals(), -1));
            assertEquals(just(entry), entry.remove("foo", Bitmap32.empty(), (x, y) -> false, -1));
        }

        @Test
        public void put() {
            assertEquals(new Entry<>("foo", 2),
                         entry.put("foo", 2, bitmap32("foo".hashCode()), objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(empty()
                                            .populateAtIndex("foo".hashCode() & 31)
                                            .populateAtIndex("bar".hashCode() & 31),
                                    new Object[]{new Entry<>("foo", 1), new Entry<>("bar", 2)}),
                         entry.put("bar", 2, bitmap32("bar".hashCode()), objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(bitmap32(1),
                                    new Object[]{new Collision<>(bitmap32("foo".hashCode()),
                                                                 ImmutableStack.of(new Entry<>("foo", 1),
                                                                                   new Entry<>("bar", 2)))}),
                         entry.put("bar", 2, empty(), objectEquals(), objectHashCode(), MAX_LEVEL));
        }
    }

    public static final class CollisionTest {

        private Collision<String, Integer> collision;

        @Before
        public void setUp() {
            collision = new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("baz", 3),
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
            assertEquals(just(1), collision.get("foo", bitmap32(0), objectEquals(), -1));
            assertEquals(just(2), collision.get("bar", bitmap32(0), objectEquals(), -1));
            assertEquals(just(3), collision.get("baz", bitmap32(0), objectEquals(), -1));
            assertEquals(nothing(), collision.get("foo", bitmap32(1), objectEquals(), -1));
            assertEquals(nothing(), collision.get("foo", bitmap32(0), (x, y) -> false, -1));
        }

        @Test
        public void put() {
            assertEquals(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("foo", -1),
                                                                        new Entry<>("bar", 2),
                                                                        new Entry<>("baz", 3))),
                         collision.put("foo", -1, bitmap32(0), objectEquals(), objectHashCode(), -1));

            assertEquals(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("qux", 0),
                                                                        new Entry<>("foo", 1),
                                                                        new Entry<>("bar", 2),
                                                                        new Entry<>("baz", 3))),
                         collision.put("qux", 0, bitmap32(0), objectEquals(), objectHashCode(), -1));

            assertEquals(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("foo", 0),
                                                                        new Entry<>("foo", 1),
                                                                        new Entry<>("bar", 2),
                                                                        new Entry<>("baz", 3))),
                         collision.put("foo", 0, bitmap32(0), (x, y) -> false, objectHashCode(), -1));
        }

        @Test
        public void remove() {
            assertEquals(just(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("bar", 2),
                                                                             new Entry<>("baz", 3)))),
                         collision.remove("foo", bitmap32(0), objectEquals(), 1));
            assertEquals(just(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("foo", 1),
                                                                             new Entry<>("baz", 3)))),
                         collision.remove("bar", bitmap32(0), objectEquals(), 1));
            assertEquals(just(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("foo", 1),
                                                                             new Entry<>("bar", 2)))),
                         collision.remove("baz", bitmap32(0), objectEquals(), 1));

            assertEquals(just(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("foo", 1),
                                                                             new Entry<>("bar", 2),
                                                                             new Entry<>("baz", 3)))),
                         collision.remove("missing", bitmap32(0), objectEquals(), 1));

            assertEquals(just(new Collision<>(bitmap32(0), ImmutableStack.of(new Entry<>("foo", 1),
                                                                             new Entry<>("bar", 2),
                                                                             new Entry<>("baz", 3)))),
                         collision.remove("foo", bitmap32(0), (x, y) -> false, 1));

            assertEquals(just(collision), collision.remove("foo", bitmap32(-1), objectEquals(), 1));

            assertEquals(just(new Entry<>("baz", 3)),
                         collision
                                 .remove("foo", bitmap32(0), objectEquals(), 1)
                                 .flatMap(c -> c.remove("bar", bitmap32(0), objectEquals(), 1)));

            assertEquals(just(new Entry<>("baz", 3)),
                         collision
                                 .remove("foo", bitmap32(0), objectEquals(), 1)
                                 .flatMap(c -> c.remove("bar", bitmap32(0), objectEquals(), 1)));
        }
    }

    public static final class NodeTest {

        private static final Integer foo  = 0b00_00000_00000_00000_00000_00000_00000;
        private static final Integer bar  = 0b00_00000_00000_00000_00000_00000_00001;
        private static final Integer baz  = 0b00_00000_00000_00000_10000_00001_00001;
        private static final Integer quux = 0b00_00000_00000_00000_10000_00001_00001;

        @Test
        public void iteratesAllEntries() {
            assertThat(new Node<>(empty(), new Object[]{
                               new Entry<>("foo", 1),
                               new Node<>(empty(), new Object[]{new Entry<>("bar", 2)}),
                               new Collision<>(empty(), ImmutableStack.of(new Entry<>("quux", 4),
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
            Node<Integer, String> node = new Node<Integer, String>(empty(), new Object[0])
                    .put(foo, "foo", bitmap32(foo), referenceEquals(), objectHashCode(), 1)
                    .put(bar, "bar", bitmap32(bar), referenceEquals(), objectHashCode(), 1)
                    .put(baz, "baz", bitmap32(baz), referenceEquals(), objectHashCode(), 1)
                    .put(quux, "quux", bitmap32(quux), referenceEquals(), objectHashCode(), 1);

            assertEquals(just("foo"), node.get(foo, bitmap32(foo), referenceEquals(), 1));
            assertEquals(just("bar"), node.get(bar, bitmap32(bar), referenceEquals(), 1));
            assertEquals(just("baz"), node.get(baz, bitmap32(baz), referenceEquals(), 1));
            assertEquals(just("quux"), node.get(quux, bitmap32(quux), referenceEquals(), 1));

            assertEquals(nothing(), node.get(-1, bitmap32(foo), referenceEquals(), 1));
            assertEquals(nothing(), node.get(foo, bitmap32(-1), referenceEquals(), 1));
        }

        @Test
        public void put() {
            assertEquals(new Node<>(bitmap32(1), new Object[]{new Entry<>(foo, "foo")}),
                         rootNode().put(foo, "foo", bitmap32(foo), objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(bitmap32(2), new Object[]{new Entry<>(bar, "bar")}),
                         rootNode().put(bar, "bar", bitmap32(bar), objectEquals(), objectHashCode(), 1));
            assertEquals(new Node<>(bitmap32(3), new Object[]{new Entry<>(foo, "foo"), new Entry<>(bar, "bar")}),
                         rootNode()
                                 .put(foo, "foo", bitmap32(foo), objectEquals(), objectHashCode(), 1)
                                 .put(bar, "bar", bitmap32(bar), objectEquals(), objectHashCode(), 1));

            assertEquals(new Node<>(bitmap32(3), new Object[]{
                                 new Entry<>(foo, "foo"),
                                 new Node<>(bitmap32(3), new Object[]{
                                         new Entry<>(bar, "bar"),
                                         new Entry<>(baz, "baz")
                                 })}),
                         rootNode()
                                 .put(foo, "foo", bitmap32(foo), objectEquals(), objectHashCode(), 1)
                                 .put(bar, "bar", bitmap32(bar), objectEquals(), objectHashCode(), 1)
                                 .put(baz, "baz", bitmap32(baz), objectEquals(), objectHashCode(), 1));

            assertEquals(
                    new Node<>(bitmap32(3), new Object[]{
                            new Entry<>(foo, "foo"),
                            new Node<>(bitmap32(3), new Object[]{
                                    new Entry<>(bar, "bar"),
                                    new Node<>(bitmap32(0b00_00000_00000_00010_00000_00000_00000), new Object[]{
                                            new Node<>(bitmap32(1), new Object[]{
                                                    new Node<>(bitmap32(1), new Object[]{
                                                            new Node<>(bitmap32(1), new Object[]{
                                                                    new Node<>(bitmap32(1), new Object[]{
                                                                            new Collision<>(
                                                                                    bitmap32(0b10000_00001_00001),
                                                                                    ImmutableStack.of(
                                                                                            new Entry<>(baz, "baz"),
                                                                                            new Entry<>(quux, "quux")))
                                                                    })
                                                            })
                                                    })
                                            })
                                    })
                            })}),
                    rootNode()
                            .put(foo, "foo", bitmap32(foo), referenceEquals(), objectHashCode(), 1)
                            .put(bar, "bar", bitmap32(bar), referenceEquals(), objectHashCode(), 1)
                            .put(baz, "baz", bitmap32(baz), referenceEquals(), objectHashCode(), 1)
                            .put(quux, "quux", bitmap32(quux), referenceEquals(), objectHashCode(), 1));
        }

        @Test
        public void remove() {
            assertEquals(just(Node.<Integer, String>rootNode()),
                         Node.<Integer, String>rootNode().remove(-1, bitmap32(-1), objectEquals(), -1));

            assertEquals(just(Node.<Integer, String>rootNode()),
                         Node.<Integer, String>rootNode()
                                 .put(foo, "foo", bitmap32(foo), objectEquals(), objectHashCode(), 1)
                                 .remove(foo, bitmap32(foo), objectEquals(), 1));

            assertEquals(just(Node.<Integer, String>rootNode()
                                      .put(foo, "foo", bitmap32(foo), objectEquals(), objectHashCode(), 1)),
                         Node.<Integer, String>rootNode()
                                 .put(foo, "foo", bitmap32(foo), objectEquals(), objectHashCode(), 1)
                                 .remove(foo, bitmap32(-1), objectEquals(), 1));

            assertEquals(just(Node.<Integer, String>rootNode()
                                      .put(bar, "bar", bitmap32(bar), objectEquals(), objectHashCode(), 1)),
                         Node.<Integer, String>rootNode()
                                 .put(foo, "foo", bitmap32(foo), objectEquals(), objectHashCode(), 1)
                                 .put(bar, "bar", bitmap32(bar), objectEquals(), objectHashCode(), 1)
                                 .remove(foo, bitmap32(foo), objectEquals(), 1));

            assertEquals(just(new Node<>(bitmap32(2), new Object[]{
                                 new Node<>(bitmap32(2), new Object[]{
                                         new Node<>(bitmap32(0b10_00000_00000_00000), new Object[]{
                                                 new Node<>(bitmap32(1), new Object[]{
                                                         new Node<>(bitmap32(1), new Object[]{
                                                                 new Node<>(bitmap32(1), new Object[]{
                                                                         new Node<>(bitmap32(1), new Object[]{
                                                                                 new Entry<>(baz, "baz")
                                                                         })
                                                                 })
                                                         })
                                                 })
                                         })
                                 })
                         })),
                         Node.<Integer, String>rootNode()
                                 .put(baz, "baz", bitmap32(baz), referenceEquals(), objectHashCode(), 1)
                                 .put(quux, "quux", bitmap32(quux), referenceEquals(), objectHashCode(), 1)
                                 .remove(quux, bitmap32(quux), referenceEquals(), 1));
        }
    }
}