package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictStack;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsSequence;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SequenceTest {

    @Test
    public void isEmpty() {
        assertTrue(new DefaultMethodsSequence<>(StrictStack.empty()).isEmpty());
        assertFalse(new DefaultMethodsSequence<>(StrictStack.of(1)).isEmpty());
    }

}