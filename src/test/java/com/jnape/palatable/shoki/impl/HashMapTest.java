package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.testsupport.StubbedHashingAlgorithm;
import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.referenceEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.identityHashCode;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.HashMap.hashMap;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.isEmpty;
import static testsupport.matchers.IterableMatcher.iterates;

public class HashMapTest {

    @Test
    public void getMissingKey() {
        assertEquals(nothing(), HashMap.<Integer, Boolean>hashMap().get(0));
    }

    @Test
    public void getPresentKey() {
        assertEquals(just(true), HashMap.<Integer, Boolean>hashMap().put(0, true).get(0));

        HashMap<Integer, Boolean> populated = HashMap.<Integer, Boolean>hashMap()
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

        HashMap<Integer, Integer> deepCollisions =
                hashMap(objectEquals(), objectHashCode(),
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
        assertEquals(just(false), HashMap.<Integer, Boolean>hashMap()
                .put(0, true)
                .put(0, false)
                .get(0));
    }

    @Test
    public void immutability() {
        HashMap<Integer, Boolean> empty = hashMap();
        assertNotSame(empty, empty.put(0, true));
        assertEquals(nothing(), empty.get(0));
    }

    @Test
    public void ofStaticFactoryMethod() {
        assertEquals(hashSet(), hashMap().keys());
        assertEquals(hashSet("foo", "bar", "baz"),
                     hashMap(tuple("foo", 1),
                             tuple("bar", 2),
                             tuple("baz", 3)).keys());

    }

    @Test
    public void keysWithPartialHashCollisionPropagateDownwards() {
        HashMap<String, Integer> nestedCollisions = hashMap(
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
        HashMap<String, Integer> collision = HashMap.<String, Integer>hashMap(
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
        HashMap<String, Integer> collision = HashMap.<String, Integer>hashMap(
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
        HashMap<Integer, String> empty = hashMap();
        assertFalse(empty.contains(0));
        assertTrue(empty.put(0, "foo").contains(0));
    }

    @Test
    public void remove() {
        HashMap<Integer, Boolean> empty = hashMap();
        assertEquals(empty, empty.remove(0));
        assertEquals(nothing(), empty.put(0, true).remove(0).get(0));
    }

    @Test
    public void removeNested() {
        HashMap<String, Integer> nested = HashMap.<String, Integer>hashMap(
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
        HashMap<String, Integer> collision = HashMap.<String, Integer>hashMap(
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
        assertEquals(known(zero()), hashMap().sizeInfo());
        assertEquals(known(one()), hashMap().put(1, 1).sizeInfo());
        HashMap<Integer, Boolean> collisionsAndNesting =
                HashMap.<Integer, Boolean>hashMap(objectEquals(),
                                                  StubbedHashingAlgorithm.<Integer>stubbedHashingAlgorithm()
                                                          .stub(0, 0b00_00000_00000_00000_00000_00000_00000)
                                                          .stub(1, 0b00_00000_00000_00000_00000_00001_00000)
                                                          .stub(2, 0b00_00000_00000_00000_00000_00001_00000))
                        .put(0, true)
                        .put(1, false)
                        .put(2, true);
        assertEquals(known(abs(3)), collisionsAndNesting.sizeInfo());
        assertEquals(known(zero()), hashMap().put(1, 1).remove(1).sizeInfo());
    }

    @Test
    public void emptyDetection() {
        assertTrue(hashMap().isEmpty());
        assertFalse(hashMap().put(1, 1).isEmpty());
        assertTrue(hashMap().put(1, 1).remove(1).isEmpty());
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("HashMap[]", hashMap().toString());
        assertEquals("HashMap[(key=value)]", hashMap().put("key", "value").toString());
        assertEquals("HashMap[(foo=foo value), (bar=bar value), (baz=baz value)]",
                     hashMap(objectEquals(),
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
        assertThat(hashMap(), isEmpty());
        assertThat(hashMap().put(0, true), iterates(tuple(0, true)));
        assertThat(hashMap(objectEquals(),
                           StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                                   .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                                   .stub("bar", 0b00_00000_00000_00000_00000_00001_00000)
                                   .stub("baz", 0b00_00000_00000_00000_00000_00001_00000))
                           .put("foo", 1)
                           .put("bar", 2)
                           .put("baz", 3),
                   iterates(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)));
    }

    @Test
    public void headEntry() {
        assertEquals(nothing(), hashMap().head());
        assertEquals(just(tuple("foo", 1)), hashMap().put("foo", 1).head());
        assertEquals(just(tuple("foo", 1)), hashMap().put("foo", 1).put("bar", 32).head());
        assertEquals(just(tuple("foo", 1)), hashMap().put("bar", 32).put("foo", 1).head());
    }

    @Test
    public void tail() {
        assertEquals(hashMap(), hashMap().tail());
        assertTrue(hashMap().put("foo", 1).tail().isEmpty());

        HashMap<Integer, Boolean> tail = HashMap.<Integer, Boolean>hashMap()
                .put(0, true)
                .put(32, false)
                .tail();
        assertEquals(nothing(), tail.get(0));
        assertEquals(just(false), tail.get(32));
    }

    @Test
    public void equalsUsesSameEntriesWithObjectEqualsForValues() {
        assertEquals(hashMap(), hashMap());
        assertEquals(hashMap().put(1, 1), hashMap().put(1, 1));
        assertEquals(hashMap().put(1, 1).put(2, 2).remove(2), hashMap().put(1, 1));

        assertNotEquals(hashMap(), hashMap().put(1, 1));
        assertNotEquals(hashMap().put(1, 1), hashMap().put(1, 2));
        assertNotEquals(hashMap().put(1, 1), hashMap().put(2, 1));
        assertNotEquals(hashMap(), new Object());
        assertNotEquals(hashMap(tuple(1, 2)), hashMap(tuple("foo", "bar")));

        assertEquals(HashMap.<Integer, Boolean>hashMap()
                             .put(0, true)
                             .put(32, false),
                     HashMap.<Integer, Boolean>hashMap()
                             .put(0, true)
                             .put(32, false)
                             .put(64, true)
                             .remove(64));
    }

    @Test
    public void hashCodeUsesKeyHashingAlgorithmForKeysAndObjectHashForValuesForEqualsSymmetry() {
        assertEquals(hashMap().hashCode(), hashMap().hashCode());
        assertEquals(hashMap().put(1, 1).hashCode(), hashMap().put(1, 1).hashCode());

        assertNotEquals(hashMap().hashCode(), hashMap().put(0, 1).hashCode());
        assertNotEquals(hashMap().put(1, 1).hashCode(), hashMap().put(1, 2).hashCode());

        assertEquals(hashMap().put(0, 1).put(32, 2).remove(32).hashCode(), hashMap().put(0, 1).hashCode());
    }

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void keys() {
        assertEquals(hashSet(), hashMap().keys());
        assertEquals(hashSet("foo", "bar", "baz"),
                     hashMap(tuple("foo", 1),
                             tuple("bar", 2),
                             tuple("baz", 3)).keys());
        Integer first  = new Integer(1);
        Integer second = new Integer(1);
        assertThat(hashMap(referenceEquals(), identityHashCode(),
                           tuple(first, "a"),
                           tuple(second, "b")).keys(),
                   equivalentTo(hashSet(referenceEquals(), identityHashCode(), first, second),
                                sameElements()));
    }

    @Test
    public void values() {
        assertEquals(strictQueue(1, 2, 3),
                     hashMap(tuple("foo", 1),
                             tuple("bar", 2),
                             tuple("baz", 3))
                             .values());
    }

    @Test
    public void removeKeyForSingleCollisionThatDoesNotContainKey() {
        assertEquals(just(1),
                     hashMap(objectEquals(),
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
        HashMap<String, Integer> doubleCollision =
                hashMap(objectEquals(),
                        StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                                .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                                .stub("bar", 0b00_00000_00000_00000_00000_00000_00000)
                                .stub("baz", 0b00_00000_00000_00000_00000_00000_00000),
                        tuple("foo", 1),
                        tuple("bar", 2),
                        tuple("foo", 3));
        assertEquals(just(3), doubleCollision.get("foo"));
        assertEquals(nothing(), doubleCollision.remove("foo").get("foo"));
        assertEquals(known(abs(2)), doubleCollision.sizeInfo());
    }
}
