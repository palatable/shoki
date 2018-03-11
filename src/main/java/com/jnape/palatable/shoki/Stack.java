package com.jnape.palatable.shoki;

/**
 * A LIFO {@link Collection}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public interface Stack<Size extends Number, A> extends Collection<Size, A> {

    /**
     * Add an element to the front of this {@link Stack}.
     *
     * @param a the element
     * @return this {@link Stack} with the new head element
     */
    Stack<Size, A> cons(A a);
}
