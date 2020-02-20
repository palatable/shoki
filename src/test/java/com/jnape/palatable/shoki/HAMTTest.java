package com.jnape.palatable.shoki;

import org.junit.Test;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.HAMT.empty;
import static org.junit.Assert.assertEquals;

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
        StubbedHash<String> foo = new StubbedHash<>("foo", 0b00_00000_00000_00000_00000_00000_00000);
        StubbedHash<String> bar = new StubbedHash<>("bar", 0b00_00000_00000_00000_00000_00001_00000);
        StubbedHash<String> baz = new StubbedHash<>("baz", 0b00_00000_00000_00000_00001_00001_00000);
        StubbedHash<String> qux = new StubbedHash<>("qux", 0b00_00000_00000_00000_00001_00000_00000);
        StubbedHash<String> zux = new StubbedHash<>("zux", 0b01_00000_00000_00000_00001_00000_00000);
        HAMT<StubbedHash<String>, Integer> nested = HAMT.<StubbedHash<String>, Integer>empty()
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
        StubbedHash<String> foo = new StubbedHash<>("foo", 0b00_00000_00000_00000_00000_00000_00000);
        StubbedHash<String> bar = new StubbedHash<>("bar", 0b00_00000_00000_00000_00000_00000_00000);
        HAMT<StubbedHash<String>, Integer> collision = HAMT.<StubbedHash<String>, Integer>empty()
                .put(foo, 0)
                .put(bar, 1);

        assertEquals(just(0), collision.get(foo));
        assertEquals(just(1), collision.get(bar));
    }

    @Test
    public void overridingAsPartOfCollision() {
        StubbedHash<String> foo = new StubbedHash<>("foo", 0b00_00000_00000_00000_00000_00000_00000);
        StubbedHash<String> bar = new StubbedHash<>("bar", 0b00_00000_00000_00000_00000_00000_00000);
        HAMT<StubbedHash<String>, Integer> collision = HAMT.<StubbedHash<String>, Integer>empty()
                .put(foo, 0)
                .put(bar, 1)
                .put(bar, 2);

        assertEquals(just(0), collision.get(foo));
        assertEquals(just(2), collision.get(bar));
    }

    static class StubbedHash<A> {
        private final A   a;
        private final int hash;

        StubbedHash(A a, int hash) {
            this.a = a;
            this.hash = hash;
        }

        @Override
        public int hashCode() {
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof StubbedHash && Objects.equals(a, ((StubbedHash<?>) obj).a);
        }
    }
}