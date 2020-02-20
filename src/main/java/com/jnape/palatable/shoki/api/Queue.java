package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.ImmutableQueue;

/**
 * A FIFO {@link Collection}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 * @see ImmutableQueue
 */
public interface Queue<Size extends Number, A> extends OrderedCollection<Size, A> {

    /**
     * Add an element to the back of this {@link Queue}.
     *
     * @param a the element
     * @return this {@link Queue} with the element added to the back
     */
    Queue<Size, A> snoc(A a);

    /**
     *
     */
    @Override
    Queue<Size, A> tail();

    /**
     *
     */
    @Override
    Queue<Size, A> reverse();
}
