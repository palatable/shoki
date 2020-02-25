package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;

/**
 * A {@link Queue} supporting element insertion or deletion from both ends.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public interface Deque<Size extends Number, A> extends Queue<Size, A>, Stack<Size, A> {

    /**
     * @inheritDoc
     */
    @Override
    Deque<Size, A> cons(A a);

    /**
     * @inheritDoc
     */
    @Override
    Deque<Size, A> snoc(A a);

    /**
     * @inheritDoc
     */
    @Override
    Deque<Size, A> tail();

    /**
     * If this {@link Deque} is not empty, return the last element wrapped in {@link Maybe#just}, according to FIFO
     * iteration order. Otherwise, return {@link Maybe#nothing()}.
     *
     * @return {@link Maybe} the last element
     */
    Maybe<A> last();

    /**
     * Return the elements of this {@link Deque} without the last element, if there is one.
     *
     * @return the init {@link Deque}
     */
    Deque<Size, A> init();

    /**
     * @inheritDoc
     */
    @Override
    Deque<Size, A> reverse();
}
