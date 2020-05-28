package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.api.Collection;
import com.jnape.palatable.shoki.api.Map;
import com.jnape.palatable.shoki.api.Set;
import com.jnape.palatable.shoki.api.SizeInfo;

/**
 * A minimum complete definition of a {@link Map}, relying on a delegate to supply the required implementation details,
 * otherwise relying on default method implementations from {@link Map}. Used to test default {@link Map} methods.
 *
 * @param <Size> the known size {@link Number} type
 * @param <K>    the key type
 * @param <V>    the value type
 */
public final class DefaultMethodsMap<Size extends Number, K, V> implements Map<Size, K, V> {
    private final Map<Size, K, V> delegate;

    private DefaultMethodsMap(Map<Size, K, V> delegate) {
        this.delegate = delegate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsMap<Size, K, V> put(K k, V v) {
        return new DefaultMethodsMap<>(delegate.put(k, v));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsMap<Size, K, V> remove(K k) {
        return new DefaultMethodsMap<>(delegate.remove(k));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Size, K> keys() {
        return delegate.keys();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<Size, V> values() {
        return delegate.values();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Maybe<Tuple2<K, V>> head() {
        return delegate.head();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DefaultMethodsMap<Size, K, V> tail() {
        return new DefaultMethodsMap<>(delegate.tail());
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
    public Maybe<V> get(K k) {
        return delegate.get(k);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return "DefaultMethodsMap{" +
                "delegate=" + delegate +
                '}';
    }

    /**
     * Static factory method for creating a new {@link DefaultMethodsMap} given a <code>delegate</code> {@link Map}.
     *
     * @param delegate the delegate {@link Map}
     * @param <Size>   the known size {@link Number} type
     * @param <K>      the key type
     * @param <V>      the value type
     * @return the new {@link DefaultMethodsMap}
     */
    public static <Size extends Number, K, V> DefaultMethodsMap<Size, K, V> delegate(Map<Size, K, V> delegate) {
        return new DefaultMethodsMap<>(delegate);
    }
}
