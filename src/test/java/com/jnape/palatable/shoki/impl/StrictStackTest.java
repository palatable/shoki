package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Replicate.replicate;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class StrictStackTest {

    @Test
    public void headOfEmptyIsNothing() {
        assertEquals(nothing(), strictStack().head());
    }

    @Test
    public void emptyStackIsEmpty() {
        assertTrue(strictStack().isEmpty());
    }

    @Test
    public void tailOfEmptyIsAlsoEmpty() {
        assertTrue(strictStack().tail().isEmpty());
    }

    @Test
    public void emptyReusesSameInstance() {
        assertSame(strictStack(), strictStack());
    }

    @Test
    public void iteratesLastInFirstOutIfNonEmpty() {
        StrictStack<Integer> strictStack = StrictStack.<Integer>strictStack().cons(3).cons(2).cons(1);

        assertEquals(just(1), strictStack.head());
        assertEquals(just(2), strictStack.tail().head());
        assertEquals(just(3), strictStack.tail().tail().head());
        assertEquals(nothing(), strictStack.tail().tail().tail().head());
    }

    @Test
    public void nonEmptyStackIsNotEmpty() {
        assertFalse(strictStack(1).isEmpty());
    }

    @Test
    public void reverse() {
        assertEquals(strictStack(3, 2, 1), strictStack(1, 2, 3).reverse());
        assertEquals(strictStack(1, 2, 3), strictStack(1, 2, 3).reverse().reverse());
    }

    @Test
    public void consAll() {
        assertEquals(strictStack(1, 2, 3), strictStack(3).consAll(strictStack(2, 1)));
    }

    @Test
    public void structureIsShared() {
        StrictStack<Integer> tail        = strictStack(3, 2);
        StrictStack<Integer> strictStack = tail.cons(1);

        assertSame(strictStack.tail(), tail);
    }

    @Test
    public void convenienceStaticFactoryMethod() {
        assertEquals(strictStack().cons(3).cons(2).cons(1), strictStack(1, 2, 3));
    }

    @Test
    public void stackSafeEqualsAndHashCode() {
        StrictStack<Integer> xs = foldLeft(StrictStack::cons, strictStack(), replicate(10_000, 1));
        StrictStack<Integer> ys = foldLeft(StrictStack::cons, strictStack(), replicate(10_000, 1));
        assertEquals(xs, ys);
        assertEquals(xs.hashCode(), ys.hashCode());
        assertEquals(strictStack(), strictStack());
    }

    @Test
    public void toStringImplementation() {
        assertEquals("StrictStack[]", strictStack().toString());
        assertEquals("StrictStack[1, 2, 3]", strictStack(1, 2, 3).toString());
    }
}