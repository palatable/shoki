package com.jnape.palatable.shoki;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Replicate.replicate;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.ImmutableQueue.empty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

public class ImmutableQueueTest {

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
        ImmutableQueue<Integer> queue = ImmutableQueue.<Integer>empty().snoc(1).snoc(2).snoc(3);
        assertEquals(just(1), queue.head());
        assertEquals(just(2), queue.tail().head());
        assertEquals(just(3), queue.tail().tail().head());
        assertEquals(nothing(), queue.tail().tail().tail().head());
    }

    @Test
    public void nonEmptyQueueQueuesIncomingElementsBehindOutgoing() {
        ImmutableQueue<Integer> outgoingQueued = ImmutableQueue.<Integer>empty().snoc(1).snoc(2).snoc(3).tail();
        ImmutableQueue<Integer> queue          = outgoingQueued.snoc(4).snoc(5);

        assertEquals(just(2), queue.head());
        assertEquals(just(3), queue.tail().head());
        assertEquals(just(4), queue.tail().tail().head());
        assertEquals(just(5), queue.tail().tail().tail().head());
        assertEquals(nothing(), queue.tail().tail().tail().tail().head());
    }

    @Test
    public void canAlsoConsElements() {
        ImmutableQueue<Integer> queue = ImmutableQueue.<Integer>empty().cons(1).cons(2).cons(3);
        assertEquals(just(3), queue.head());
        assertEquals(just(2), queue.tail().head());
        assertEquals(just(1), queue.tail().tail().head());
        assertEquals(nothing(), queue.tail().tail().tail().head());
    }

    @Test
    public void reverse() {
        assertEquals(ImmutableQueue.of(3, 2, 1), ImmutableQueue.of(1, 2, 3).reverse());
        assertEquals(ImmutableQueue.of(5, 4, 3, 2, 1), ImmutableQueue.of(2, 3).tail().snoc(4).snoc(5).cons(2).cons(1).reverse());
        assertEquals(ImmutableQueue.of(1, 2, 3), ImmutableQueue.of(1, 2, 3).reverse().reverse());

        assertEquals(ImmutableQueue.of(1), ImmutableQueue.of(1).reverse());
    }

    @Test
    public void stackSafeEqualsAndHashCode() {
        ImmutableQueue<Integer> xs = foldLeft(ImmutableQueue::cons, empty(), replicate(10_000, 1));
        ImmutableQueue<Integer> ys = foldLeft(ImmutableQueue::cons, empty(), replicate(10_000, 1));
        assertEquals(xs, ys);
        assertEquals(xs.hashCode(), ys.hashCode());
        assertEquals(empty(), empty());
        assertNotEquals(ImmutableQueue.of(1), ImmutableQueue.of(2));
    }

    @Test
    public void toStringImplementation() {
        assertEquals("ImmutableQueue[]", empty().toString());
        assertEquals("ImmutableQueue[1, 2, 3]", ImmutableQueue.of(1, 2, 3).toString());
    }
}