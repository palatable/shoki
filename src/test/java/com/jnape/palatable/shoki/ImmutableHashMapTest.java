package com.jnape.palatable.shoki;

import com.jnape.palatable.shoki.testsupport.StubbedHashingAlgorithm;
import org.junit.Test;

import java.util.HashMap;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.ImmutableHashMap.empty;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static org.junit.Assert.*;
import static testsupport.matchers.IterableMatcher.isEmpty;
import static testsupport.matchers.IterableMatcher.iterates;

public class ImmutableHashMapTest {

    @Test
    public void getMissingKey() {
        assertEquals(nothing(), ImmutableHashMap.<Integer, Boolean>empty().get(0));
    }

    @Test
    public void getPresentKey() {
        assertEquals(just(true), ImmutableHashMap.<Integer, Boolean>empty().put(0, true).get(0));
    }

    @Test
    public void putReplacementKey() {
        assertEquals(just(false), ImmutableHashMap.<Integer, Boolean>empty()
            .put(0, true)
            .put(0, false)
            .get(0));
    }

    @Test
    public void immutability() {
        ImmutableHashMap<Integer, Boolean> empty = empty();
        assertNotSame(empty, empty.put(0, true));
        assertEquals(nothing(), empty.get(0));
    }

    @Test
    public void keysWithPartialHashCollisionPropagateDownwards() {
        ImmutableHashMap<String, Integer> nested = ImmutableHashMap.<String, Integer>empty(
            objectEquals(), StubbedHashingAlgorithm.<String>stubbedHashingAlgorithm()
                .stub("foo", 0b00_00000_00000_00000_00000_00000_00000)
                .stub("bar", 0b00_00000_00000_00000_00000_00001_00000)
                .stub("baz", 0b00_00000_00000_00000_00001_00001_00000)
                .stub("qux", 0b00_00000_00000_00000_00001_00000_00000)
                .stub("zux", 0b01_00000_00000_00000_00001_00000_00000))
            .put("foo", 1)
            .put("bar", 2)
            .put("baz", 3)
            .put("qux", 4)
            .put("zux", 5);

        assertEquals(just(1), nested.get("foo"));
        assertEquals(just(2), nested.get("bar"));
        assertEquals(just(3), nested.get("baz"));
        assertEquals(just(4), nested.get("qux"));
        assertEquals(just(5), nested.get("zux"));
    }

    @Test
    public void keysWithFullCollisionsAreStoredAdjacently() {
        ImmutableHashMap<String, Integer> collision = ImmutableHashMap.<String, Integer>empty(
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
        ImmutableHashMap<String, Integer> collision = ImmutableHashMap.<String, Integer>empty(
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
        ImmutableHashMap<Integer, String> empty = empty();
        assertFalse(empty.contains(0));
        assertTrue(empty.put(0, "foo").contains(0));
    }

    @Test
    public void remove() {
        ImmutableHashMap<Integer, Boolean> empty = empty();
        assertEquals(empty, empty.remove(0));
        assertEquals(nothing(), empty.put(0, true).remove(0).get(0));
    }

    @Test
    public void removeNested() {
        ImmutableHashMap<String, Integer> nested = ImmutableHashMap.<String, Integer>empty(
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
        ImmutableHashMap<String, Integer> collision = ImmutableHashMap.<String, Integer>empty(
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
    public void buildingFromJavaMap() {
        ImmutableHashMap<Integer, Boolean> immutableHashMap = ImmutableHashMap.fromJavaMap(new HashMap<Integer,
            Boolean>() {{
            put(0, true);
            put(1, false);
            put(2, true);
        }});

        assertEquals(just(true), immutableHashMap.get(0));
        assertEquals(just(false), immutableHashMap.get(1));
        assertEquals(just(true), immutableHashMap.get(2));
        assertEquals(nothing(), immutableHashMap.get(3));
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(0), empty().sizeInfo());
        assertEquals(known(1), ImmutableHashMap.empty().put(1, 1).sizeInfo());
        ImmutableHashMap<Integer, Boolean> collisionsAndNesting =
            ImmutableHashMap.<Integer, Boolean>empty(objectEquals(),
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

    @Test(timeout = 1000)
    public void insertionSanityBenchmark() {
        int                                n                = 1_000_000;
        ImmutableHashMap<Integer, Boolean> immutableHashMap = empty();
        for (int i = 0; i < n; i++) {
            immutableHashMap = immutableHashMap.put(i, true);
        }
    }

    @Test
    public void emptyDetection() {
        assertTrue(empty().isEmpty());
        assertFalse(empty().put(1, 1).isEmpty());
        assertTrue(empty().put(1, 1).remove(1).isEmpty());
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("ImmutableHashMap{entries=[]}", empty().toString());
        assertEquals("ImmutableHashMap{entries=[(k=key, v=value)]}", empty().put("key", "value").toString());
        assertEquals("ImmutableHashMap{entries=[(k=foo, v=foo value) | (k=baz, v=baz value) | (k=bar, v=bar value)]}",
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

        ImmutableHashMap<Integer, Boolean> tail = ImmutableHashMap.<Integer, Boolean>empty()
            .put(0, true)
            .put(32, false)
            .tail();
        assertEquals(nothing(), tail.get(0));
        assertEquals(just(false), tail.get(32));
    }

    @Test
    public void keys() {

    }
}
