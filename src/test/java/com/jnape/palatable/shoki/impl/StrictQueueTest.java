package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Replicate.replicate;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.impl.StrictQueue.empty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

public class StrictQueueTest {

    @Test
    public void headIfEmptyIsNothing() {
        assertEquals(nothing(), empty().head());
    }

    @Test
    public void isEmptyIfEmpty() {
        assertTrue(empty().isEmpty());
    }

    @Test
    public void tailIfEmptyIsAlsoEmpty() {
        assertTrue(empty().tail().isEmpty());
    }

    @Test
    public void nonEmptyQueueIsNotEmpty() {
        assertFalse(empty().snoc(1).isEmpty());
    }

    @Test
    public void nonEmptyQueueIteratesElementsFirstInFirstOut() {
        StrictQueue<Integer> strictQueue = StrictQueue.<Integer>empty().snoc(1).snoc(2).snoc(3);
        assertEquals(just(1), strictQueue.head());
        assertEquals(just(2), strictQueue.tail().head());
        assertEquals(just(3), strictQueue.tail().tail().head());
        assertEquals(nothing(), strictQueue.tail().tail().tail().head());
    }

    @Test
    public void nonEmptyQueueQueuesIncomingElementsBehindOutgoing() {
        StrictQueue<Integer> outgoingQueued = StrictQueue.<Integer>empty().snoc(1).snoc(2).snoc(3).tail();
        StrictQueue<Integer> strictQueue    = outgoingQueued.snoc(4).snoc(5);

        assertEquals(just(2), strictQueue.head());
        assertEquals(just(3), strictQueue.tail().head());
        assertEquals(just(4), strictQueue.tail().tail().head());
        assertEquals(just(5), strictQueue.tail().tail().tail().head());
        assertEquals(nothing(), strictQueue.tail().tail().tail().tail().head());
    }

    @Test
    public void canAlsoConsElements() {
        StrictQueue<Integer> strictQueue = StrictQueue.<Integer>empty().cons(1).cons(2).cons(3);
        assertEquals(just(3), strictQueue.head());
        assertEquals(just(2), strictQueue.tail().head());
        assertEquals(just(1), strictQueue.tail().tail().head());
        assertEquals(nothing(), strictQueue.tail().tail().tail().head());
    }

    @Test
    public void reverse() {
        assertEquals(StrictQueue.of(3, 2, 1), StrictQueue.of(1, 2, 3).reverse());
        assertEquals(StrictQueue.of(5, 4, 3, 2, 1), StrictQueue.of(2, 3).tail().snoc(4).snoc(5).cons(2).cons(1).reverse());
        assertEquals(StrictQueue.of(1, 2, 3), StrictQueue.of(1, 2, 3).reverse().reverse());

        assertEquals(StrictQueue.of(1), StrictQueue.of(1).reverse());
        assertEquals(StrictQueue.empty(), StrictQueue.empty().reverse());
    }

    @Test
    public void stackSafeEqualsAndHashCode() {
        StrictQueue<Integer> xs = foldLeft(StrictQueue::cons, empty(), replicate(10_000, 1));
        StrictQueue<Integer> ys = foldLeft(StrictQueue::cons, empty(), replicate(10_000, 1));
        assertEquals(xs, ys);
        assertEquals(xs.hashCode(), ys.hashCode());
        assertEquals(empty(), empty());
        assertNotEquals(StrictQueue.of(1), StrictQueue.of(2));
    }

    @Test
    public void toStringImplementation() {
        assertEquals("StrictQueue[]", empty().toString());
        assertEquals("StrictQueue[1, 2, 3]", StrictQueue.of(1, 2, 3).toString());
    }
}