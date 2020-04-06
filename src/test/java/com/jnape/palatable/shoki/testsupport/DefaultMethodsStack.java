package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.SizeInfo;
import com.jnape.palatable.shoki.api.Stack;

/**
 * A minimum complete definition of a {@link Stack}, relying on a delegate to supply the required implementation
 * details, otherwise relying on default method implementations from {@link Stack}. Used to test default {@link Stack}
 * methods.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public final class DefaultMethodsStack<Size extends Number, A> implements Stack<Size, A> {
    private final Stack<Size, A> delegate;

    private DefaultMethodsStack(Stack<Size, A> delegate) {
        this.delegate = delegate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsStack<Size, A> cons(A a) {
        return new DefaultMethodsStack<>(delegate.cons(a));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsStack<Size, A> tail() {
        return new DefaultMethodsStack<>(delegate.tail());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsStack<Size, A> reverse() {
        return new DefaultMethodsStack<>(delegate.reverse());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SizeInfo.Known<Size> sizeInfo() {
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
     * Static factory method for creating a new {@link DefaultMethodsStack} given a <code>delegate</code> {@link Stack}.
     *
     * @param delegate the delegate {@link Stack}
     * @param <Size>   the known size {@link Number} type
     * @param <A>      the element type
     * @return the new {@link DefaultMethodsStack}
     */
    public static <Size extends Number, A> DefaultMethodsStack<Size, A> delegate(Stack<Size, A> delegate) {
        return new DefaultMethodsStack<>(delegate);
    }
}
