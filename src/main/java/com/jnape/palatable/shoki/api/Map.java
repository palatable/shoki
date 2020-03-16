package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;

/**
 * A {@link Collection} of key/value {@link Tuple2 pairs} supporting {@link RandomAccess random access} from a key to
 * {@link Maybe} a value.
 *
 * @param <Size> the known size {@link Number} type
 * @param <K>    the key type
 * @param <V>    the value type
 */
public interface Map<Size extends Number, K, V> extends
        Collection<Size, Tuple2<K, V>>,
        RandomAccess<K, Maybe<V>> {

    /**
     * Associate <code>k</code> with <code>v</code> inside this map.
     *
     * @param k the key
     * @param v the value
     * @return the updated {@link Map}
     */
    Map<Size, K, V> put(K k, V v);

    /**
     * Remove all associations for <code>k</code> inside this map.
     *
     * @param k the key
     * @return the updated {@link Map}
     */
    Map<Size, K, V> remove(K k);

    /**
     * The {@link Set set} of all keys associated to a value inside this {@link Map}.
     *
     * @return this {@link Map Map's} {@link Set key set}
     */
    Set<Size, K> keys();

    /**
     * A {@link Collection} of all values associated to a key inside this {@link Map Map}.
     *
     * @return the {@link Map Map's} {@link Collection} of values
     */
    Collection<Size, V> values();

    /**
     * {@inheritDoc}
     *
     * @return this {@link Map} without the {@link Map#head() head} key/value {@link Tuple2 pair}
     */
    @Override
    Map<Size, K, V> tail();
}
