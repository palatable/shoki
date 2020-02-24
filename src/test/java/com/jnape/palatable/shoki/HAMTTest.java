package com.jnape.palatable.shoki;

import org.junit.Test;

import java.util.HashMap;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.HAMT.empty;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static org.junit.Assert.*;

public class HAMTTest {

    @Test
    public void getMissingKey() {
        assertEquals(nothing(), HAMT.<Integer, Boolean>empty().get(0));
    }

    @Test
    public void getPresentKey() {
        assertEquals(just(true), HAMT.<Integer, Boolean>empty().put(0, true).get(0));
    }

    @Test
    public void putReplacementKey() {
        assertEquals(just(false), HAMT.<Integer, Boolean>empty()
            .put(0, true)
            .put(0, false)
            .get(0));
    }

    @Test
    public void immutability() {
        HAMT<Integer, Boolean> empty = empty();
        assertNotSame(empty, empty.put(0, true));
        assertEquals(nothing(), empty.get(0));
    }

    @Test
    public void keysWithPartialHashCollisionPropagateDownwards() {
        HAMT<String, Integer> nested = HAMT.<String, Integer>empty(
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
        HAMT<String, Integer> collision = HAMT.<String, Integer>empty(
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
        HAMT<String, Integer> collision = HAMT.<String, Integer>empty(
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
        HAMT<Integer, String> empty = empty();
        assertFalse(empty.contains(0));
        assertTrue(empty.put(0, "foo").contains(0));
    }

    @Test
    public void remove() {
        HAMT<Integer, Boolean> empty = empty();
        assertEquals(empty, empty.remove(0));
        assertEquals(nothing(), empty.put(0, true).remove(0).get(0));
    }

    @Test
    public void removeNested() {
        HAMT<String, Integer> nested = HAMT.<String, Integer>empty(
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
        HAMT<String, Integer> collision = HAMT.<String, Integer>empty(
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
        HAMT<Integer, Boolean> hamt = HAMT.fromJavaMap(new HashMap<Integer, Boolean>() {{
            put(0, true);
            put(1, false);
            put(2, true);
        }});

        assertEquals(just(true), hamt.get(0));
        assertEquals(just(false), hamt.get(1));
        assertEquals(just(true), hamt.get(2));
        assertEquals(nothing(), hamt.get(3));
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(0), empty().sizeInfo());
        assertEquals(known(1), HAMT.empty().put(1, 1).sizeInfo());
        HAMT<Integer, Boolean> collisionsAndNesting =
            HAMT.<Integer, Boolean>empty(objectEquals(), StubbedHashingAlgorithm.<Integer>stubbedHashingAlgorithm()
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
        int                    n    = 1_000_000;
        HAMT<Integer, Boolean> hamt = empty();
        for (int i = 0; i < n; i++) {
            hamt = hamt.put(i, true);
        }
    }

    public static final class StubbedHashingAlgorithm<A> implements HashingAlgorithm<A> {
        private final HAMT<A, Integer> table;

        private StubbedHashingAlgorithm(HAMT<A, Integer> table) {
            this.table = table;
        }

        @Override
        public Integer checkedApply(A a) {
            return table.get(a).orElseGet(a::hashCode);
        }

        public StubbedHashingAlgorithm<A> stub(A a, Integer hash) {
            return new StubbedHashingAlgorithm<>(table.put(a, hash));
        }

        public static <A> StubbedHashingAlgorithm<A> stubbedHashingAlgorithm() {
            return new StubbedHashingAlgorithm<>(empty());
        }
    }
}