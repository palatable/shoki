package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.ImmutableQueue;
import com.jnape.palatable.shoki.ImmutableStack;

/**
 * A LIFO {@link Collection}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 * @see ImmutableStack
 * @see ImmutableQueue
 */
public interface Stack<Size extends Number, A> extends OrderedCollection<Size, A> {

    /**
     * Add an element to the front of this {@link Stack}.
     *
     * @param a the element
     * @return this {@link Stack} with the new head element
     */
    Stack<Size, A> cons(A a);

    /**
     * @inheritDoc
     */
    @Override
    Stack<Size, A> tail();

    /**
     * @inheritDoc
     */
    @Override
    Stack<Size, A> reverse();
}
