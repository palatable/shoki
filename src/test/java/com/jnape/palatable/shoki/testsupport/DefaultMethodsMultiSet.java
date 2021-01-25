package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.api.MultiSet;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Set;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;
import com.jnape.palatable.shoki.api.Value;

/**
 * A minimum complete definition of a {@link MultiSet}, relying on a delegate to supply the required implementation
 * details, otherwise relying on default method implementations from {@link MultiSet}. Used to test default
 * {@link MultiSet} methods.
 *
 * @param <A> the element type
 */
public final class DefaultMethodsMultiSet<A> implements MultiSet<A> {
    private final MultiSet<A> delegate;

    private DefaultMethodsMultiSet(MultiSet<A> delegate) {
        this.delegate = delegate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsMultiSet<A> inc(A a, Natural.NonZero k) {
        return new DefaultMethodsMultiSet<>(delegate.inc(a, k));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsMultiSet<A> dec(A a, Natural.NonZero k) {
        return new DefaultMethodsMultiSet<>(delegate.dec(a, k));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Natural get(A a) {
        return delegate.get(a);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Value<Finite<Natural>> sizeInfo() {
        return delegate.sizeInfo();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsMultiSet<A> tail() {
        return new DefaultMethodsMultiSet<>(delegate.tail());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(A a) {
        return delegate.contains(a);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Natural, A> unique() {
        return delegate.unique();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Maybe<Tuple2<A, Natural.NonZero>> head() {
        return delegate.head();
    }

    /**
     * Static factory method for creating a new {@link DefaultMethodsMultiSet} given a <code>delegate</code>
     * {@link MultiSet}.
     *
     * @param delegate the delegate {@link MultiSet}
     * @param <A>      the element type
     * @return the new {@link DefaultMethodsMultiSet}
     */
    public static <A> DefaultMethodsMultiSet<A> delegate(MultiSet<A> delegate) {
        return new DefaultMethodsMultiSet<>(delegate);
    }
}
