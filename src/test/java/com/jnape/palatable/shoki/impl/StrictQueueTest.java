package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static com.jnape.palatable.shoki.impl.AmortizedStack.amortizedStack;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

public class StrictQueueTest {

    @Test
    public void headIfEmptyIsNothing() {
        assertEquals(nothing(), strictQueue().head());
    }

    @Test
    public void isEmptyIfEmpty() {
        assertTrue(strictQueue().isEmpty());
    }

    @Test
    public void tailIfEmptyIsAlsoEmpty() {
        assertTrue(strictQueue().tail().isEmpty());
    }

    @Test
    public void nonEmptyQueueIsNotEmpty() {
        assertFalse(strictQueue().snoc(1).isEmpty());
    }

    @Test
    public void nonEmptyQueueIteratesElementsFirstInFirstOut() {
        StrictQueue<Integer> strictQueue = StrictQueue.<Integer>strictQueue().snoc(1).snoc(2).snoc(3);
        assertEquals(just(1), strictQueue.head());
        assertEquals(just(2), strictQueue.tail().head());
        assertEquals(just(3), strictQueue.tail().tail().head());
        assertEquals(nothing(), strictQueue.tail().tail().tail().head());
    }

    @Test
    public void nonEmptyQueueQueuesIncomingElementsBehindOutgoing() {
        StrictQueue<Integer> outgoingQueued = StrictQueue.<Integer>strictQueue().snoc(1).snoc(2).snoc(3).tail();
        StrictQueue<Integer> strictQueue    = outgoingQueued.snoc(4).snoc(5);

        assertEquals(just(2), strictQueue.head());
        assertEquals(just(3), strictQueue.tail().head());
        assertEquals(just(4), strictQueue.tail().tail().head());
        assertEquals(just(5), strictQueue.tail().tail().tail().head());
        assertEquals(nothing(), strictQueue.tail().tail().tail().tail().head());
    }

    @Test
    public void canAlsoConsElements() {
        StrictQueue<Integer> strictQueue = StrictQueue.<Integer>strictQueue().cons(1).cons(2).cons(3);
        assertEquals(just(3), strictQueue.head());
        assertEquals(just(2), strictQueue.tail().head());
        assertEquals(just(1), strictQueue.tail().tail().head());
        assertEquals(nothing(), strictQueue.tail().tail().tail().head());
    }

    @Test
    public void reverse() {
        assertEquals(strictQueue(3, 2, 1), strictQueue(1, 2, 3).reverse());
        assertEquals(strictQueue(5, 4, 3, 2, 1),
                     strictQueue(2, 3).tail().snoc(4).snoc(5).cons(2).cons(1).reverse());
        assertEquals(strictQueue(1, 2, 3), strictQueue(1, 2, 3).reverse().reverse());

        assertEquals(strictQueue(1), strictQueue(1).reverse());
        assertEquals(strictQueue(), strictQueue().reverse());
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(strictQueue(), strictQueue());
        assertEquals(strictQueue(1, 2, 3), strictQueue(1, 2, 3));
        assertNotEquals(strictQueue(1), strictQueue(2));
        assertNotEquals(strictQueue(1), new Object());

        assertEquals(strictQueue().hashCode(), strictQueue().hashCode());
        assertEquals(strictQueue(1, 2, 3).hashCode(), strictQueue(1, 2, 3).hashCode());
        assertNotEquals(strictQueue(1, 2, 3).hashCode(), strictQueue(3, 2, 1).hashCode());
    }

    @Test
    public void toStringImplementation() {
        assertEquals("StrictQueue[]", strictQueue().toString());
        assertEquals("StrictQueue[1, 2, 3]", strictQueue(1, 2, 3).toString());
    }

    @Test
    public void consAll() {
        assertEquals(strictQueue(1, 2, 3, 4, 5), strictQueue(4, 5).consAll(amortizedStack(3, 2, 1)));
    }

    @Test
    public void snocAll() {
        assertEquals(strictQueue(1, 2, 3, 4, 5), strictQueue(1, 2).snocAll(amortizedStack(3, 4, 5)));
    }
}