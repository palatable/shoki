package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictStack;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsStack;
import org.junit.Test;

import static com.jnape.palatable.shoki.impl.StrictStack.empty;
import static org.junit.Assert.assertTrue;

public class StackTest {

    @Test
    public void consAll() {
        assertTrue(OrderedCollection.equals(DefaultMethodsStack.delegate(empty()).consAll(StrictStack.of(1, 2, 3)),
                                            StrictStack.of(3, 2, 1)));
        assertTrue(OrderedCollection.equals(DefaultMethodsStack.delegate(StrictStack.of(5, 4))
                                                    .consAll(StrictStack.of(1, 2, 3)),
                                            StrictStack.of(5, 4, 3, 2, 1)));
        assertTrue(OrderedCollection.equals(DefaultMethodsStack.delegate(StrictStack.of(5, 4))
                                                    .consAll(StrictStack.empty()),
                                            StrictStack.of(5, 4)));
    }
}