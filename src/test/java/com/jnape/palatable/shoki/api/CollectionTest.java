package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictStack;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsCollection;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class CollectionTest {

    @Test
    public void isEmpty() {
        assertTrue(DefaultMethodsCollection.delegate(StrictStack.empty()).isEmpty());
        assertFalse(DefaultMethodsCollection.delegate(StrictStack.of(1)).isEmpty());
    }
}