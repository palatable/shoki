package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.Sequence;

/**
 * A minimum complete definition of a {@link Sequence}, relying on a delegate to supply the required implementation
 * details, otherwise relying on default method implementations from {@link Sequence}. Used to test default
 * {@link Sequence} methods.
 *
 * @param <A> the element type
 */
public final class DefaultMethodsSequence<A> implements Sequence<A> {

    private final Sequence<A> delegate;

    public DefaultMethodsSequence(Sequence<A> delegate) {
        this.delegate = delegate;
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
    public DefaultMethodsSequence<A> tail() {
        return delegate(delegate.tail());
    }

    /**
     * Static factory method for creating a new {@link DefaultMethodsSequence} given a <code>delegate</code>
     * {@link Sequence}.
     *
     * @param delegate the delegate {@link Sequence}
     * @param <A>      the element type
     * @return the new {@link DefaultMethodsSequence}
     */
    public static <A> DefaultMethodsSequence<A> delegate(Sequence<A> delegate) {
        return new DefaultMethodsSequence<>(delegate);
    }
}
