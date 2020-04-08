package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Replicate.replicate;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class StrictStackTest {

    @Test
    public void headIfEmptyIsNothing() {
        assertEquals(nothing(), StrictStack.empty().head());
    }

    @Test
    public void emptyStackIsEmpty() {
        assertTrue(StrictStack.empty().isEmpty());
    }

    @Test
    public void tailOfEmptyIsAlsoEmpty() {
        assertTrue(StrictStack.empty().tail().isEmpty());
    }

    @Test
    public void emptyReusesSameInstance() {
        assertSame(StrictStack.empty(), StrictStack.empty());
    }

    @Test
    public void pop() {
        assertEquals(just(tuple(1, StrictStack.of(3, 2))), StrictStack.of(3, 2, 1).pop());
        assertEquals(nothing(), StrictStack.empty().pop());
    }

    @Test
    public void iteratesLastInFirstOutIfNonEmpty() {
        StrictStack<Integer> strictStack = StrictStack.<Integer>empty().cons(3).cons(2).cons(1);

        assertEquals(just(1), strictStack.head());
        assertEquals(just(2), strictStack.tail().head());
        assertEquals(just(3), strictStack.tail().tail().head());
        assertEquals(nothing(), strictStack.tail().tail().tail().head());
    }

    @Test
    public void nonEmptyStackIsNotEmpty() {
        assertFalse(StrictStack.empty().cons(1).isEmpty());
    }

    @Test
    public void reverse() {
        assertEquals(StrictStack.of(3, 2, 1), StrictStack.of(1, 2, 3).reverse());
        assertEquals(StrictStack.of(1, 2, 3), StrictStack.of(1, 2, 3).reverse().reverse());
    }

    @Test
    public void structureIsShared() {
        StrictStack<Integer> tail        = StrictStack.of(3, 2);
        StrictStack<Integer> strictStack = tail.cons(1);

        assertSame(strictStack.tail(), tail);
    }

    @Test
    public void convenienceStaticFactoryMethod() {
        assertEquals(StrictStack.empty().cons(1).cons(2).cons(3), StrictStack.of(1, 2, 3));
    }

    @Test
    public void stackSafeEqualsAndHashCode() {
        StrictStack<Integer> xs = foldLeft(StrictStack::cons, StrictStack.empty(), replicate(10_000, 1));
        StrictStack<Integer> ys = foldLeft(StrictStack::cons, StrictStack.empty(), replicate(10_000, 1));
        assertEquals(xs, ys);
        assertEquals(xs.hashCode(), ys.hashCode());
        assertEquals(StrictStack.empty(), StrictStack.empty());
    }

    @Test
    public void toStringImplementation() {
        assertEquals("StrictStack[]", StrictStack.empty().toString());
        assertEquals("StrictStack[1, 2, 3]", StrictStack.of(3, 2, 1).toString());
    }
}