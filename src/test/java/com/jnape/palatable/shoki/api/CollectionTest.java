package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.testsupport.DefaultMethodsCollection;
import org.junit.Test;

import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class CollectionTest {

    @Test
    public void isEmpty() {
        assertTrue(DefaultMethodsCollection.delegate(strictStack()).isEmpty());
        assertFalse(DefaultMethodsCollection.delegate(strictStack(1)).isEmpty());
    }
}