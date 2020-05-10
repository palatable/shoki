package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Replicate.replicate;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
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
    public void stackSafeEqualsAndHashCode() {
        StrictQueue<Integer> xs = foldLeft(StrictQueue::cons, strictQueue(), replicate(10_000, 1));
        StrictQueue<Integer> ys = foldLeft(StrictQueue::cons, strictQueue(), replicate(10_000, 1));
        assertEquals(xs, ys);
        assertEquals(xs.hashCode(), ys.hashCode());
        assertEquals(strictQueue(), strictQueue());
        assertNotEquals(strictQueue(1), strictQueue(2));
    }

    @Test
    public void toStringImplementation() {
        assertEquals("StrictQueue[]", strictQueue().toString());
        assertEquals("StrictQueue[1, 2, 3]", strictQueue(1, 2, 3).toString());
    }

    @Test
    public void consAll() {
        assertEquals(strictQueue(1, 2, 3, 4, 5), strictQueue(4, 5).consAll(strictStack(3, 2, 1)));
    }

    @Test
    public void snocAll() {
        assertEquals(strictQueue(1, 2, 3, 4, 5), strictQueue(1, 2).snocAll(strictStack(3, 4, 5)));
    }
}