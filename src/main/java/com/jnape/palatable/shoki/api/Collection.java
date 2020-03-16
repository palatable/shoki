package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.SizeInfo.Known;

/**
 * A known-sized {@link Sequence}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public interface Collection<Size extends Number, A> extends Sequence<A>, Sizable {

    /**
     * The known size of this collection.
     *
     * @return the size of this collection
     */
    @Override
    Known<Size> sizeInfo();

    /**
     * {@inheritDoc}
     */
    @Override
    Collection<Size, A> tail();

    /**
     * {@inheritDoc}
     * <p>
     * {@link Collection} implementations by default use {@link Collection#sizeInfo()} to determine emptiness.
     */
    @Override
    default boolean isEmpty() {
        return sizeInfo().getSize().intValue() == 0;
    }
}
