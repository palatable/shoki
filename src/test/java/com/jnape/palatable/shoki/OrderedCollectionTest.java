package com.jnape.palatable.shoki;

import com.jnape.palatable.shoki.api.OrderedCollection;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class OrderedCollectionTest {

    @Test
    public void equality() {
        OrderedCollection<?, Integer> stack = ImmutableStack.of(1, 2, 3);
        OrderedCollection<?, Integer> queue = ImmutableQueue.of(3, 2, 1);

        assertTrue(OrderedCollection.equals(stack, queue));
        assertTrue(OrderedCollection.equals(queue, stack));
        assertTrue(OrderedCollection.equals(queue, queue));
        assertTrue(OrderedCollection.equals(stack, stack));

        assertFalse(OrderedCollection.equals(ImmutableStack.of(3, 2, 1), stack));
        assertFalse(OrderedCollection.equals(stack, ImmutableStack.of(3, 2, 1)));
        assertFalse(OrderedCollection.equals(queue, ImmutableStack.of(3, 2, 1)));
        assertFalse(OrderedCollection.equals(ImmutableStack.of(3, 2, 1), queue));
        assertFalse(OrderedCollection.equals(ImmutableQueue.of(1, 2, 3), queue));
        assertFalse(OrderedCollection.equals(queue, ImmutableQueue.of(1, 2, 3)));
        assertFalse(OrderedCollection.equals(stack, ImmutableQueue.of(1, 2, 3)));
        assertFalse(OrderedCollection.equals(ImmutableQueue.of(1, 2, 3), stack));
    }
}