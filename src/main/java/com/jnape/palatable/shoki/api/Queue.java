package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictQueue;

import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;

/**
 * A {@link Collection} offering <em>first-in, first-out</em> semantics.
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

    /**
     * {@link Queue#snoc(Object) Snoc} each element in <code>collection</code> (from front to back) onto the back of
     * this {@link Queue}.
     *
     * @param collection the {@link Collection} from which to {@link Queue#snoc(Object) snoc} elements
     * @return the updated {@link Queue}
     */
    default Queue<Size, A> snocAll(Collection<Size, A> collection) {
        return foldLeft(Queue<Size, A>::snoc, this, collection);
    }
}
