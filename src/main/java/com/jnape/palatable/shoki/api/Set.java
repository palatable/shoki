package com.jnape.palatable.shoki.api;

/**
 * A {@link Set} is a {@link Collection} of distinct elements with a {@link Membership} capability for determining if
 * an element is a member of the {@link Set}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public interface Set<Size extends Number, A> extends Collection<Size, A>, Membership<A> {

    /**
     * Include <code>a</code> as a member of this {@link Set}.
     *
     * @param a the element
     * @return the updated {@link Set}
     */
    Set<Size, A> add(A a);

    /**
     * Exclude <code>a</code> as a member of this {@link Set}.
     *
     * @param a the element
     * @return the updated {@link Set}
     */
    Set<Size, A> remove(A a);

    /**
     * {@inheritDoc}
     *
     * @return this {@link Set} without the {@link Set#head() head} element's membership
     */
    @Override
    Set<Size, A> tail();
}
