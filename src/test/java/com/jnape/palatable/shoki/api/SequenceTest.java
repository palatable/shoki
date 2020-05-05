package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.testsupport.DefaultMethodsSequence;
import org.junit.Test;

import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SequenceTest {

    @Test
    public void isEmpty() {
        assertTrue(new DefaultMethodsSequence<>(strictStack()).isEmpty());
        assertFalse(new DefaultMethodsSequence<>(strictStack(1)).isEmpty());
    }
}