package com.jnape.palatable.shoki;

/**
 * A FIFO {@link Collection}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public interface Queue<Size extends Number, A> extends Collection<Size, A> {

    /**
     * Add an element to the back of this {@link Queue}.
     *
     * @param a the element
     * @return this {@link Queue} with the element added to the back
     */
    Queue<Size, A> snoc(A a);
}
