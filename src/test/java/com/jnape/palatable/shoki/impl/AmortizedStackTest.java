package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.impl.AmortizedStack.amortizedStack;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class AmortizedStackTest {

    @Test
    public void headOfEmptyIsNothing() {
        assertEquals(nothing(), amortizedStack().head());
    }

    @Test
    public void emptyStackIsEmpty() {
        assertTrue(amortizedStack().isEmpty());
    }

    @Test
    public void tailOfEmptyIsAlsoEmpty() {
        assertTrue(amortizedStack().tail().isEmpty());
    }

    @Test
    public void emptyReusesSameInstance() {
        assertSame(amortizedStack(), amortizedStack());
    }

    @Test
    public void iteratesLastInFirstOutIfNonEmpty() {
        AmortizedStack<Integer> amortizedStack = AmortizedStack.<Integer>amortizedStack().cons(3).cons(2).cons(1);

        assertEquals(just(1), amortizedStack.head());
        assertEquals(just(2), amortizedStack.tail().head());
        assertEquals(just(3), amortizedStack.tail().tail().head());
        assertEquals(nothing(), amortizedStack.tail().tail().tail().head());
    }

    @Test
    public void nonEmptyStackIsNotEmpty() {
        assertFalse(amortizedStack(1).isEmpty());
    }

    @Test
    public void reverse() {
        assertEquals(amortizedStack(3, 2, 1), amortizedStack(1, 2, 3).reverse());
        assertEquals(amortizedStack(1, 2, 3), amortizedStack(1, 2, 3).reverse().reverse());
    }

    @Test
    public void consAll() {
        assertEquals(amortizedStack(1, 2, 3), amortizedStack(3).consAll(amortizedStack(2, 1)));
    }

    @Test
    public void structureIsShared() {
        AmortizedStack<Integer> tail           = amortizedStack(3, 2);
        AmortizedStack<Integer> amortizedStack = tail.cons(1);

        assertSame(amortizedStack.tail(), tail);
    }

    @Test
    public void convenienceStaticFactoryMethod() {
        assertEquals(amortizedStack().cons(3).cons(2).cons(1), amortizedStack(1, 2, 3));
    }

    @Test
    public void equalsAndHashCode() {
        AmortizedStack<Integer> xs = amortizedStack(1, 2, 3);
        AmortizedStack<Integer> ys = amortizedStack(1, 2, 3);
        assertEquals(xs, ys);
        assertEquals(xs.hashCode(), ys.hashCode());
        assertEquals(amortizedStack(), amortizedStack());
    }

    @Test
    public void toStringImplementation() {
        assertEquals("AmortizedStack[]", amortizedStack().toString());
        assertEquals("AmortizedStack[1, 2, 3]", amortizedStack(1, 2, 3).toString());
    }
}