package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class OrderedCollectionTest {

    @Test
    public void equality() {
        OrderedCollection<?, Integer> stack = StrictStack.of(1, 2, 3);
        OrderedCollection<?, Integer> queue = StrictQueue.of(3, 2, 1);

        assertTrue(OrderedCollection.equals(stack, queue));
        assertTrue(OrderedCollection.equals(queue, stack));
        assertTrue(OrderedCollection.equals(queue, queue));
        assertTrue(OrderedCollection.equals(stack, stack));

        assertFalse(OrderedCollection.equals(StrictStack.of(3, 2, 1), stack));
        assertFalse(OrderedCollection.equals(stack, StrictStack.of(3, 2, 1)));
        assertFalse(OrderedCollection.equals(queue, StrictStack.of(3, 2, 1)));
        assertFalse(OrderedCollection.equals(StrictStack.of(3, 2, 1), queue));
        assertFalse(OrderedCollection.equals(StrictQueue.of(1, 2, 3), queue));
        assertFalse(OrderedCollection.equals(queue, StrictQueue.of(1, 2, 3)));
        assertFalse(OrderedCollection.equals(stack, StrictQueue.of(1, 2, 3)));
        assertFalse(OrderedCollection.equals(StrictQueue.of(1, 2, 3), stack));
        assertFalse(OrderedCollection.equals(StrictQueue.of(1, 2, 3), StrictQueue.empty()));
    }
}