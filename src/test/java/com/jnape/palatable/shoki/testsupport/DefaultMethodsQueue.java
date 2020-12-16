package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.Queue;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;

/**
 * A minimum complete definition of a {@link Queue}, relying on a delegate to supply the required implementation
 * details, otherwise relying on default method implementations from {@link Queue}. Used to test default {@link Queue}
 * methods.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public final class DefaultMethodsQueue<Size extends Number, A> implements Queue<Size, A> {
    private final Queue<Size, A> delegate;

    private DefaultMethodsQueue(Queue<Size, A> delegate) {
        this.delegate = delegate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsQueue<Size, A> snoc(A a) {
        return new DefaultMethodsQueue<>(delegate.snoc(a));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsQueue<Size, A> tail() {
        return new DefaultMethodsQueue<>(delegate.tail());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsQueue<Size, A> reverse() {
        return new DefaultMethodsQueue<>(delegate.reverse());
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
    public Maybe<A> head() {
        return delegate.head();
    }

    /**
     * Static factory method for creating a new {@link DefaultMethodsQueue} given a <code>delegate</code> {@link Queue}.
     *
     * @param delegate the delegate {@link Queue}
     * @param <Size>   the known size {@link Number} type
     * @param <A>      the element type
     * @return the new {@link DefaultMethodsQueue}
     */
    public static <Size extends Number, A> DefaultMethodsQueue<Size, A> delegate(Queue<Size, A> delegate) {
        return new DefaultMethodsQueue<>(delegate);
    }
}
