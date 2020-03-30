package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictQueue;

/**
 * A {@link Collection} offering "first-in/first-out" semantics..
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 * @see StrictQueue
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
     * {@inheritDoc}
     */
    @Override
    Queue<Size, A> tail();

    /**
     * {@inheritDoc}
     */
    @Override
    Queue<Size, A> reverse();
}
