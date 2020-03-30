package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class CollectionTest {

    @Test
    public void isEmpty() {
        Collection<Integer, String> empty = new Collection<Integer, String>() {
            @Override
            public SizeInfo.Known<Integer> sizeInfo() {
                return SizeInfo.known(0);
            }

            @Override
            public Collection<Integer, String> tail() {
                return this;
            }

            @Override
            public Maybe<String> head() {
                return Maybe.nothing();
            }
        };
        assertTrue(empty.isEmpty());
    }
}