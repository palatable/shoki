package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.Set;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;
import com.jnape.palatable.shoki.api.Value;

/**
 * A minimum complete definition of a {@link Set}, relying on a delegate to supply the required implementation details,
 * otherwise relying on default method implementations from {@link Set}. Used to test default {@link Set} methods.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public final class DefaultMethodsSet<Size extends Number, A> implements Set<Size, A> {
    private final Set<Size, A> delegate;

    private DefaultMethodsSet(Set<Size, A> delegate) {
        this.delegate = delegate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsSet<Size, A> add(A a) {
        return delegate(delegate.add(a));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsSet<Size, A> remove(A a) {
        return delegate(delegate.remove(a));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Value<Finite<Size>> sizeInfo() {
        return delegate.sizeInfo();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Maybe<A> head() {
        return delegate.head();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsSet<Size, A> tail() {
        return delegate(delegate.tail());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(A a) {
        return delegate.contains(a);
    }

    /**
     * Static factory method for creating a new {@link DefaultMethodsSet} given a <code>delegate</code> {@link Set}.
     *
     * @param delegate the delegate {@link Set}
     * @param <Size>   the known size {@link Number} type
     * @param <A>      the element type
     * @return the new {@link DefaultMethodsSet}
     */
    public static <Size extends Number, A> DefaultMethodsSet<Size, A> delegate(Set<Size, A> delegate) {
        return new DefaultMethodsSet<>(delegate);
    }
}
