package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;

import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;

/**
 * A {@link Collection} offering <em>last-in, first-out</em> semantics.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 * @see StrictStack
 * @see StrictQueue
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
     * {@inheritDoc}
     */
    @Override
    Stack<Size, A> tail();

    /**
     * {@inheritDoc}
     */
    @Override
    Stack<Size, A> reverse();

    /**
     * {@link Stack#cons(Object) Cons} each element in <code>collection</code> (from front to back) onto the front of
     * this {@link Stack}.
     *
     * @param other the {@link Collection} from which to {@link Stack#cons(Object) cons} elements
     * @return the updated {@link Stack}
     */
    default Stack<Size, A> consAll(Collection<Size, A> other) {
        return foldLeft(Stack<Size, A>::cons, this, other);
    }
}
