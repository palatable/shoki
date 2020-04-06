package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsQueue;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class QueueTest {

    @Test
    public void snocAll() {
        assertTrue(OrderedCollection.equals(DefaultMethodsQueue.delegate(StrictQueue.empty())
                                                    .snocAll(StrictQueue.of(1, 2, 3)),
                                            StrictQueue.of(1, 2, 3)));
        assertTrue(OrderedCollection.equals(DefaultMethodsQueue.delegate(StrictQueue.of(-1, 0))
                                                    .snocAll(StrictQueue.of(1, 2, 3)),
                                            StrictQueue.of(-1, 0, 1, 2, 3)));
        assertTrue(OrderedCollection.equals(DefaultMethodsQueue.delegate(StrictQueue.of(-1, 0))
                                                    .snocAll(StrictQueue.empty()),
                                            StrictQueue.of(-1, 0)));
    }
}