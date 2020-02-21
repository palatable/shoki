package com.jnape.palatable.shoki;

import org.junit.Test;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.HAMT.empty;
import static com.jnape.palatable.shoki.HAMTTest.Hashed.hashed;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
        empty.put(0, true);
        assertEquals(nothing(), empty.get(0));
    }

    @Test
    public void keysWithPartialHashCollisionPropagateDownwards() {
        Hashed<String> foo = hashed("foo", 0b00_00000_00000_00000_00000_00000_00000);
        Hashed<String> bar = hashed("bar", 0b00_00000_00000_00000_00000_00001_00000);
        Hashed<String> baz = hashed("baz", 0b00_00000_00000_00000_00001_00001_00000);
        Hashed<String> qux = hashed("qux", 0b00_00000_00000_00000_00001_00000_00000);
        Hashed<String> zux = hashed("zux", 0b01_00000_00000_00000_00001_00000_00000);
        HAMT<Hashed<String>, Integer> nested = HAMT.<Hashed<String>, Integer>empty()
                .put(foo, 1)
                .put(bar, 2)
                .put(baz, 3)
                .put(qux, 4)
                .put(zux, 5);

        assertEquals(just(1), nested.get(foo));
        assertEquals(just(2), nested.get(bar));
        assertEquals(just(3), nested.get(baz));
        assertEquals(just(4), nested.get(qux));
        assertEquals(just(5), nested.get(zux));
    }

    @Test
    public void keysWithFullCollisionsAreStoredAdjacently() {
        Hashed<String> foo = hashed("foo", 0b00_00000_00000_00000_00000_00000_00000);
        Hashed<String> bar = hashed("bar", 0b00_00000_00000_00000_00000_00000_00000);
        HAMT<Hashed<String>, Integer> collision = HAMT.<Hashed<String>, Integer>empty()
                .put(foo, 0)
                .put(bar, 1);

        assertEquals(just(0), collision.get(foo));
        assertEquals(just(1), collision.get(bar));
    }

    @Test
    public void overridingAsPartOfCollision() {
        Hashed<String> foo = hashed("foo", 0b00_00000_00000_00000_00000_00000_00000);
        Hashed<String> bar = hashed("bar", 0b00_00000_00000_00000_00000_00000_00000);
        HAMT<Hashed<String>, Integer> collision = HAMT.<Hashed<String>, Integer>empty()
                .put(foo, 0)
                .put(bar, 1)
                .put(bar, 2);

        assertEquals(just(0), collision.get(foo));
        assertEquals(just(2), collision.get(bar));
    }

    @Test
    public void contains() {
        HAMT<Integer, String> empty = empty();
        assertFalse(empty.contains(0));
        assertTrue(empty.put(0, "foo").contains(0));
    }

    @Test(timeout = 1000)
    public void insertionSanityBenchmark() {
        int                    n    = 1_000_000;
        HAMT<Integer, Integer> hamt = empty();
        for (int i = 0; i < n; i++) {
            hamt = hamt.put(i, i);
        }
    }

    public static final class Hashed<A> {
        private final A   a;
        private final int hash;

        private Hashed(A a, int hash) {
            this.a = a;
            this.hash = hash;
        }

        @Override
        public int hashCode() {
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof Hashed && Objects.equals(a, ((Hashed<?>) obj).a);
        }

        public static <A> Hashed<A> hashed(A a, int hash) {
            return new Hashed<>(a, hash);
        }
    }
}