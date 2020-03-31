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

        assertFalse(OrderedCollection.equals(stack.reverse(), stack));
        assertFalse(OrderedCollection.equals(stack, stack.reverse()));
        assertFalse(OrderedCollection.equals(queue, stack.reverse()));
        assertFalse(OrderedCollection.equals(stack.reverse(), queue));
        assertFalse(OrderedCollection.equals(queue.reverse(), queue));
        assertFalse(OrderedCollection.equals(queue, queue.reverse()));
        assertFalse(OrderedCollection.equals(stack, queue.reverse()));
        assertFalse(OrderedCollection.equals(queue.reverse(), stack));
        assertFalse(OrderedCollection.equals(queue.reverse(), StrictQueue.empty()));
    }
}