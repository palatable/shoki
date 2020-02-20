package com.jnape.palatable.shoki;

import org.junit.Test;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
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
    public void keysWithPartialHashCollisionPropagateDownwards() {
        class StubbedHash<A> {
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

        StubbedHash<String> foo = new StubbedHash<>("foo", 0b00_00000_00000_00000_00000_00000_00000);
        StubbedHash<String> bar = new StubbedHash<>("bar", 0b00_00000_00000_00000_00000_00001_00000);
        HAMT<StubbedHash<String>, Integer> nested = HAMT.<StubbedHash<String>, Integer>empty()
                .put(foo, 1)
                .put(bar, 2);

        assertEquals(just(1), nested.get(foo));
        assertEquals(just(2), nested.get(bar));
    }
}