package com.jnape.palatable.shoki.hamt;

import com.jnape.palatable.shoki.ImmutableHashSet;
import com.jnape.palatable.shoki.ImmutableStack;
import com.jnape.palatable.shoki.testsupport.StubbedHashingAlgorithm;
import org.junit.Assert;
import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.hamt.HashArrayMappedTrie.empty;
import static com.jnape.palatable.shoki.hamt.HashArrayMappedTrie.hashArrayMappedTrie;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.isEmpty;
import static testsupport.matchers.IterableMatcher.iterates;

public class HashArrayMappedTrieTest {

    @Test
    public void getMissingKey() {
        assertEquals(nothing(), HashArrayMappedTrie.<Integer, Boolean>empty().get(0));
    }

    @Test
    public void getPresentKey() {
        assertEquals(just(true), HashArrayMappedTrie.<Integer, Boolean>empty().put(0, true).get(0));

        HashArrayMappedTrie<Integer, Boolean> populated = HashArrayMappedTrie.<Integer, Boolean>empty()
                .put(0, true)
                .put(1, false);
        assertEquals(just(true), populated.get(0));
        assertEquals(just(false), populated.get(1));
    }

    @Test
    public void deepCollisions() {
        int a = 0b00_00000_00000_00000_00000_00000_00000;
        int b = 0b01_00000_00000_00000_00000_00000_00000;
        int c = 0b01_00001_00000_00000_00000_00000_00000;
        int d = 0b01_00001_00001_00000_00000_00000_00000;
        int e = 0b01_00001_00001_00001_00000_00000_00000;
        int f = 0b01_00001_00001_00001_00001_00000_00000;
        int g = 0b01_00001_00001_00001_00001_00001_00000;
        int h = 0b01_00001_00001_00001_00001_00001_00001;

        HashArrayMappedTrie<Integer, Integer> deepCollisions =
                hashArrayMappedTrie(objectEquals(), objectHashCode(),
                                    tuple(a, 1),
                                    tuple(b, 2),
                                    tuple(c, 3),
                                    tuple(d, 4),
                                    tuple(e, 5),
                                    tuple(f, 6),
                                    tuple(g, 7),
                                    tuple(h, 8));

        assertEquals(deepCollisions.get(a), just(1));
        assertEquals(deepCollisions.get(b), just(2));
        assertEquals(deepCollisions.get(c), just(3));
        assertEquals(deepCollisions.get(d), just(4));
        assertEquals(deepCollisions.get(e), just(5));
        assertEquals(deepCollisions.get(f), just(6));
        assertEquals(deepCollisions.get(g), just(7));
        assertEquals(deepCollisions.get(h), just(8));
    }

    @Test
    public void putReplacementKey() {
        assertEquals(just(false), HashArrayMappedTrie.<Integer, Boolean>empty()
                .put(0, true)
                .put(0, false)
                .get(0));
    }

    @Test
    public void immutability() {
        HashArrayMappedTrie<Integer, Boolean> empty = empty();
        assertNotSame(empty, empty.put(0, true));
        assertEquals(nothing(), empty.get(0));
    }

    @Test
    public void ofStaticFactoryMethod() {
        Assert.assertEquals(ImmutableHashSet.empty(), HashArrayMappedTrie.empty().keys());
        assertEquals(ImmutableHashSet.of("foo", "bar", "baz"),
                     hashArrayMappedTrie(tuple("foo", 1),
                                         tuple("bar", 2),
                                         tuple("baz", 3)).keys());

    }

    @Test
    public void keysWithPartialHashCollisionPropagateDownwards() {
        HashArrayMappedTrie<String, Integer> nestedCollisions = hashArrayMappedTrie(
                objectEquals(), StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                        .stub("a", 0b00_00000_00000_00000_00000_00000_00000)
                        .stub("b", 0b00_00000_00000_00000_00000_00001_00000)
                        .stub("c", 0b00_00000_00000_00000_00001_00001_00000)
                        .stub("d", 0b00_00000_00000_00000_00001_00000_00000)
                        .stub("e", 0b01_00000_00000_00000_00001_00000_00000),
                tuple("a", 1),
                tuple("b", 2),
                tuple("c", 3),
                tuple("d", 4),
                tuple("e", 5));

        assertEquals(just(1), nestedCollisions.get("a"));
        assertEquals(just(2), nestedCollisions.get("b"));
        assertEquals(just(3), nestedCollisions.get("c"));
        assertEquals(just(4), nestedCollisions.get("d"));
        assertEquals(just(5), nestedCollisions.get("e"));
    }

    @Test
    public void keysWithFullCollisionsAreStoredAdjacently() {
        HashArrayMappedTrie<String, Integer> collision = HashArrayMappedTrie.<String, Integer>empty(
                objectEquals(), StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                        .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                        .stub("bar", 0b00_00000_00000_00000_00000_00000_00000))
                .put("foo", 0)
                .put("bar", 1);

        assertEquals(just(0), collision.get("foo"));
        assertEquals(just(1), collision.get("bar"));
    }

    @Test
    public void overridingAsPartOfCollision() {
        HashArrayMappedTrie<String, Integer> collision = HashArrayMappedTrie.<String, Integer>empty(
                objectEquals(), StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                        .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                        .stub("bar", 0b00_00000_00000_00000_00000_00000_00000))
                .put("foo", 0)
                .put("bar", 1)
                .put("bar", 2);

        assertEquals(just(0), collision.get("foo"));
        assertEquals(just(2), collision.get("bar"));
    }

    @Test
    public void contains() {
        HashArrayMappedTrie<Integer, String> empty = empty();
        assertFalse(empty.contains(0));
        assertTrue(empty.put(0, "foo").contains(0));
    }

    @Test
    public void remove() {
        HashArrayMappedTrie<Integer, Boolean> empty = empty();
        assertEquals(empty, empty.remove(0));
        assertEquals(nothing(), empty.put(0, true).remove(0).get(0));
    }

    @Test
    public void removeNested() {
        HashArrayMappedTrie<String, Integer> nested = HashArrayMappedTrie.<String, Integer>empty(
                objectEquals(), StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                        .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                        .stub("bar", 0b00_00000_00000_00000_00000_00001_00000))
                .put("foo", 0)
                .put("bar", 1)
                .remove("foo");
        assertEquals(just(1), nested.get("bar"));
        assertEquals(nothing(), nested.get("foo"));
    }

    @Test
    public void removeFromCollision() {
        HashArrayMappedTrie<String, Integer> collision = HashArrayMappedTrie.<String, Integer>empty(
                objectEquals(), StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                        .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                        .stub("bar", 0b00_00000_00000_00000_00000_00000_00000))
                .put("foo", 0)
                .put("bar", 1)
                .remove("foo");

        assertEquals(nothing(), collision.get("foo"));
        assertEquals(just(1), collision.get("bar"));
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(0), empty().sizeInfo());
        assertEquals(known(1), HashArrayMappedTrie.empty().put(1, 1).sizeInfo());
        HashArrayMappedTrie<Integer, Boolean> collisionsAndNesting =
                HashArrayMappedTrie.<Integer, Boolean>empty(objectEquals(),
                                                            StubbedHashingAlgorithm.<Integer>stubbedHashingAlgorithm()
                                                                    .stub(0, 0b00_00000_00000_00000_00000_00000_00000)
                                                                    .stub(1, 0b00_00000_00000_00000_00000_00001_00000)
                                                                    .stub(2, 0b00_00000_00000_00000_00000_00001_00000))
                        .put(0, true)
                        .put(1, false)
                        .put(2, true);
        assertEquals(known(3), collisionsAndNesting.sizeInfo());
        assertEquals(known(0), empty().put(1, 1).remove(1).sizeInfo());
    }

    @Test
    public void emptyDetection() {
        assertTrue(empty().isEmpty());
        assertFalse(empty().put(1, 1).isEmpty());
        assertTrue(empty().put(1, 1).remove(1).isEmpty());
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("HashArrayMappedTrie[]", empty().toString());
        assertEquals("HashArrayMappedTrie[(key=value)]", empty().put("key", "value").toString());
        assertEquals("HashArrayMappedTrie[(foo=foo value), (baz=baz value), (bar=bar value)]",
                     empty(objectEquals(),
                           StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                                   .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                                   .stub("bar", 0b00_00000_00000_00000_00000_00001_00000)
                                   .stub("baz", 0b00_00000_00000_00000_00000_00001_00000))
                             .put("foo", "foo value")
                             .put("bar", "bar value")
                             .put("baz", "baz value")
                             .toString());
    }

    @Test
    public void iterableOverEntries() {
        assertThat(empty(), isEmpty());
        assertThat(empty().put(0, true), iterates(tuple(0, true)));
        assertThat(empty(objectEquals(),
                         StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                                 .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                                 .stub("bar", 0b00_00000_00000_00000_00000_00001_00000)
                                 .stub("baz", 0b00_00000_00000_00000_00000_00001_00000))
                           .put("foo", 1)
                           .put("bar", 2)
                           .put("baz", 3),
                   iterates(tuple("foo", 1), tuple("baz", 3), tuple("bar", 2)));
    }

    @Test
    public void headEntry() {
        assertEquals(nothing(), empty().head());
        assertEquals(just(tuple("foo", 1)), empty().put("foo", 1).head());
        assertEquals(just(tuple("foo", 1)), empty().put("foo", 1).put("bar", 32).head());
        assertEquals(just(tuple("foo", 1)), empty().put("bar", 32).put("foo", 1).head());
    }

    @Test
    public void tail() {
        assertEquals(empty(), empty().tail());
        assertTrue(empty().put("foo", 1).tail().isEmpty());

        HashArrayMappedTrie<Integer, Boolean> tail = HashArrayMappedTrie.<Integer, Boolean>empty()
                .put(0, true)
                .put(32, false)
                .tail();
        assertEquals(nothing(), tail.get(0));
        assertEquals(just(false), tail.get(32));
    }


    @Test
    public void equalsUsesSameEntriesWithObjectEqualsForValues() {
        assertEquals(empty(), empty());
        assertEquals(empty().put(1, 1), empty().put(1, 1));
        assertEquals(empty().put(1, 1).put(2, 2).remove(2), empty().put(1, 1));

        assertNotEquals(empty(), empty().put(1, 1));
        assertNotEquals(empty().put(1, 1), empty().put(1, 2));
        assertNotEquals(empty().put(1, 1), empty().put(2, 1));

        assertEquals(HashArrayMappedTrie.<Integer, Boolean>empty()
                             .put(0, true)
                             .put(32, false),
                     HashArrayMappedTrie.<Integer, Boolean>empty()
                             .put(0, true)
                             .put(32, false)
                             .put(64, true)
                             .remove(64));
    }

    @Test
    public void hashCodeUsesKeyHashingAlgorithmForKeysAndObjectHashForValuesForEqualsSymmetry() {
        assertEquals(empty().hashCode(), empty().hashCode());
        assertEquals(empty().put(1, 1).hashCode(), empty().put(1, 1).hashCode());

        assertNotEquals(empty().hashCode(), empty().put(1, 1).hashCode());
        assertNotEquals(empty().put(1, 1).hashCode(), empty().put(1, 2).hashCode());

        assertEquals(empty().put(0, 1).put(32, 2).remove(32).hashCode(), empty().put(0, 1).hashCode());
    }

    @Test
    public void keys() {
        assertEquals(ImmutableHashSet.empty(), HashArrayMappedTrie.empty().keys());
        assertEquals(ImmutableHashSet.of("foo", "bar", "baz"),
                     hashArrayMappedTrie(tuple("foo", 1),
                                         tuple("bar", 2),
                                         tuple("baz", 3)).keys());
    }

    @Test
    public void values() {
        assertEquals(ImmutableStack.of(1, 2, 3),
                     hashArrayMappedTrie(tuple("foo", 1),
                                         tuple("bar", 2),
                                         tuple("baz", 3))
                             .values());
    }

    @Test
    public void removeKeyForSingleCollisionThatDoesNotContainKey() {
        assertEquals(just(1),
                     hashArrayMappedTrie(objectEquals(),
                                         StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                                                 .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                                                 .stub("bar", 0b00_00000_00000_00000_00000_00000_00000)
                                                 .stub("baz", 0b00_00000_00000_00000_00000_00000_00000),
                                         tuple("foo", 1),
                                         tuple("bar", 2))
                             .remove("bar")
                             .remove("baz")
                             .get("foo"));
    }

    @Test
    public void insertCollisionThatAlreadyExists() {
        HashArrayMappedTrie<String, Integer> doubleCollision =
                hashArrayMappedTrie(objectEquals(),
                                    StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                                            .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                                            .stub("bar", 0b00_00000_00000_00000_00000_00000_00000)
                                            .stub("baz", 0b00_00000_00000_00000_00000_00000_00000),
                                    tuple("foo", 1),
                                    tuple("bar", 2),
                                    tuple("foo", 3));
        assertEquals(just(3), doubleCollision.get("foo"));
        assertEquals(nothing(), doubleCollision.remove("foo").get("foo"));
        assertEquals(known(2), doubleCollision.sizeInfo());
    }

    @Test(timeout = 1000)
    public void insertionSanityBenchmark() {
        int                                   n                   = 1_000_000;
        HashArrayMappedTrie<Integer, Boolean> hashArrayMappedTrie = empty();
        for (int i = 0; i < n; i++) {
            hashArrayMappedTrie = hashArrayMappedTrie.put(i, true);
        }
    }
}
