package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher;
import com.jnape.palatable.shoki.testsupport.StubbedHashingAlgorithm;
import org.junit.Assert;
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
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.HashMap.empty;
import static com.jnape.palatable.shoki.impl.HashMap.of;
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
        assertEquals(nothing(), HashMap.<Integer, Boolean>empty().get(0));
    }

    @Test
    public void getPresentKey() {
        assertEquals(just(true), HashMap.<Integer, Boolean>empty().put(0, true).get(0));

        HashMap<Integer, Boolean> populated = HashMap.<Integer, Boolean>empty()
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
                HashMap.of(objectEquals(), objectHashCode(),
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
        assertEquals(just(false), HashMap.<Integer, Boolean>empty()
                .put(0, true)
                .put(0, false)
                .get(0));
    }

    @Test
    public void immutability() {
        HashMap<Integer, Boolean> empty = empty();
        assertNotSame(empty, empty.put(0, true));
        assertEquals(nothing(), empty.get(0));
    }

    @Test
    public void ofStaticFactoryMethod() {
        Assert.assertEquals(HashSet.empty(), HashMap.empty().keys());
        assertEquals(HashSet.of("foo", "bar", "baz"),
                     of(tuple("foo", 1),
                        tuple("bar", 2),
                        tuple("baz", 3)).keys());

    }

    @Test
    public void keysWithPartialHashCollisionPropagateDownwards() {
        HashMap<String, Integer> nestedCollisions = HashMap.of(
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
        HashMap<String, Integer> collision = HashMap.<String, Integer>empty(
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
        HashMap<String, Integer> collision = HashMap.<String, Integer>empty(
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
        HashMap<Integer, String> empty = empty();
        assertFalse(empty.contains(0));
        assertTrue(empty.put(0, "foo").contains(0));
    }

    @Test
    public void remove() {
        HashMap<Integer, Boolean> empty = empty();
        assertEquals(empty, empty.remove(0));
        assertEquals(nothing(), empty.put(0, true).remove(0).get(0));
    }

    @Test
    public void removeNested() {
        HashMap<String, Integer> nested = HashMap.<String, Integer>empty(
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
        HashMap<String, Integer> collision = HashMap.<String, Integer>empty(
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
        assertEquals(known(zero()), empty().sizeInfo());
        assertEquals(known(one()), HashMap.empty().put(1, 1).sizeInfo());
        HashMap<Integer, Boolean> collisionsAndNesting =
                HashMap.<Integer, Boolean>empty(objectEquals(),
                                                StubbedHashingAlgorithm.<Integer>stubbedHashingAlgorithm()
                                                        .stub(0, 0b00_00000_00000_00000_00000_00000_00000)
                                                        .stub(1, 0b00_00000_00000_00000_00000_00001_00000)
                                                        .stub(2, 0b00_00000_00000_00000_00000_00001_00000))
                        .put(0, true)
                        .put(1, false)
                        .put(2, true);
        assertEquals(known(abs(3)), collisionsAndNesting.sizeInfo());
        assertEquals(known(zero()), empty().put(1, 1).remove(1).sizeInfo());
    }

    @Test
    public void emptyDetection() {
        assertTrue(empty().isEmpty());
        assertFalse(empty().put(1, 1).isEmpty());
        assertTrue(empty().put(1, 1).remove(1).isEmpty());
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("HashMap[]", empty().toString());
        assertEquals("HashMap[(key=value)]", empty().put("key", "value").toString());
        assertEquals("HashMap[(foo=foo value), (bar=bar value), (baz=baz value)]",
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
                   iterates(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)));
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

        HashMap<Integer, Boolean> tail = HashMap.<Integer, Boolean>empty()
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

        assertEquals(HashMap.<Integer, Boolean>empty()
                             .put(0, true)
                             .put(32, false),
                     HashMap.<Integer, Boolean>empty()
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

    @SuppressWarnings("UnnecessaryBoxing")
    @Test
    public void keys() {
        assertEquals(HashSet.empty(), HashMap.empty().keys());
        assertEquals(HashSet.of("foo", "bar", "baz"),
                     of(tuple("foo", 1),
                        tuple("bar", 2),
                        tuple("baz", 3)).keys());
        Integer first  = new Integer(1);
        Integer second = new Integer(1);
        assertThat(of(EquivalenceRelation.referenceEquals(),
                      HashingAlgorithm.identityHashCode(),
                      tuple(first, "a"),
                      tuple(second, "b")).keys(),
                   EquivalenceRelationMatcher
                           .equivalentTo(HashSet.of(referenceEquals(), identityHashCode(), first, second),
                                         HashSet.EquivalenceRelations.sameElements()));
    }

    @Test
    public void values() {
        assertEquals(StrictQueue.of(1, 2, 3),
                     of(tuple("foo", 1),
                        tuple("bar", 2),
                        tuple("baz", 3))
                             .values());
    }

    @Test
    public void removeKeyForSingleCollisionThatDoesNotContainKey() {
        assertEquals(just(1),
                     HashMap.of(objectEquals(),
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
                HashMap.of(objectEquals(),
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
