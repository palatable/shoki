package com.jnape.palatable.shoki;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Replicate.replicate;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class ImmutableStackTest {

    @Test
    public void headIfEmptyIsNothing() {
        assertEquals(nothing(), ImmutableStack.empty().head());
    }

    @Test
    public void emptyStackIsEmpty() {
        assertTrue(ImmutableStack.empty().isEmpty());
    }

    @Test
    public void tailOfEmptyIsAlsoEmpty() {
        assertTrue(ImmutableStack.empty().tail().isEmpty());
    }

    @Test
    public void emptyReusesSameInstance() {
        assertSame(ImmutableStack.empty(), ImmutableStack.empty());
    }

    @Test
    public void unshift() {
        assertEquals(just(tuple(1, ImmutableStack.of(3, 2))), ImmutableStack.of(3, 2, 1).pop());
        assertEquals(nothing(), ImmutableStack.empty().pop());
    }

    @Test
    public void iteratesLastInFirstOutIfNonEmpty() {
        ImmutableStack<Integer> stack = ImmutableStack.<Integer>empty().cons(3).cons(2).cons(1);

        assertEquals(just(1), stack.head());
        assertEquals(just(2), stack.tail().head());
        assertEquals(just(3), stack.tail().tail().head());
        assertEquals(nothing(), stack.tail().tail().tail().head());
    }

    @Test
    public void nonEmptyStackIsNotEmpty() {
        assertFalse(ImmutableStack.empty().cons(1).isEmpty());
    }

    @Test
    public void reverse() {
        assertEquals(ImmutableStack.of(3, 2, 1), ImmutableStack.of(1, 2, 3).reverse());
        assertEquals(ImmutableStack.of(1, 2, 3), ImmutableStack.of(1, 2, 3).reverse().reverse());
    }

    @Test
    public void structureIsShared() {
        ImmutableStack<Integer> tail  = ImmutableStack.of(3, 2);
        ImmutableStack<Integer> stack = tail.cons(1);

        assertSame(stack.tail(), tail);
    }

    @Test
    public void convenienceStaticFactoryMethod() {
        assertEquals(ImmutableStack.empty().cons(1).cons(2).cons(3), ImmutableStack.of(asList(1, 2, 3)));
        assertEquals(ImmutableStack.empty().cons(1).cons(2).cons(3), ImmutableStack.of(1, 2, 3));
    }

    @Test
    public void stackSafeEqualsAndHashCode() {
        ImmutableStack<Integer> xs = foldLeft(ImmutableStack::cons, ImmutableStack.<Integer>empty(), replicate(10_000, 1));
        ImmutableStack<Integer> ys = foldLeft(ImmutableStack::cons, ImmutableStack.<Integer>empty(), replicate(10_000, 1));
        assertEquals(xs, ys);
        assertEquals(xs.hashCode(), ys.hashCode());
        assertEquals(ImmutableStack.empty(), ImmutableStack.empty());
    }

    @Test
    public void toStringImplementation() {
        assertEquals("ImmutableStack[]", ImmutableStack.empty().toString());
        assertEquals("ImmutableStack[1, 2, 3]", ImmutableStack.of(3, 2, 1).toString());
    }
}