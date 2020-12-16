package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.Collection;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;

/**
 * A minimum complete definition of a {@link Collection}, relying on a delegate to supply the required implementation
 * details, otherwise relying on default method implementations from {@link Collection}. Used to test default
 * {@link Collection} methods.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public final class DefaultMethodsCollection<Size extends Number, A> implements Collection<Size, A> {

    private final Collection<Size, A> delegate;

    private DefaultMethodsCollection(Collection<Size, A> delegate) {
        this.delegate = delegate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Finite<Size> sizeInfo() {
        return delegate.sizeInfo();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsCollection<Size, A> tail() {
        return delegate(delegate.tail());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Maybe<A> head() {
        return delegate.head();
    }

    /**
     * Static factory method for creating a new {@link DefaultMethodsCollection} given a <code>delegate</code>
     * {@link Collection}.
     *
     * @param delegate the delegate {@link Collection}
     * @param <Size>   the known size {@link Number} type
     * @param <A>      the element type
     * @return the new {@link DefaultMethodsCollection}
     */
    public static <Size extends Number, A> DefaultMethodsCollection<Size, A> delegate(
            Collection<Size, A> delegate) {
        return new DefaultMethodsCollection<>(delegate);
    }
}
