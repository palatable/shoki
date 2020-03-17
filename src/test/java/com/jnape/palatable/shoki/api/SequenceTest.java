package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class SequenceTest {

    @Test
    public void isEmpty() {
        Sequence<String> empty = new Sequence<String>() {
            @Override
            public Maybe<String> head() {
                return Maybe.nothing();
            }

            @Override
            public Sequence<String> tail() {
                return this;
            }
        };
        assertTrue(empty.isEmpty());
    }
}