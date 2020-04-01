package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;

import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.monoid.builtin.And.and;

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
     * @see Map#values()
     */
    Set<Size, K> keys();

    /**
     * A {@link Collection} of all values associated to a key inside this {@link Map Map}.
     *
     * @return the {@link Map Map's} {@link Collection} of values
     * @see Map#keys()
     */
    Collection<Size, V> values();

    /**
     * {@inheritDoc}
     *
     * @return the head key/value {@link Tuple2 pair}
     */
    @Override
    Maybe<Tuple2<K, V>> head();

    /**
     * {@inheritDoc}
     *
     * @return this {@link Map} without the {@link Map#head() head} key/value {@link Tuple2 pair}
     */
    @Override
    Map<Size, K, V> tail();

    /**
     * Determine if two {@link Map}s have the same {@link SizeInfo}, and contain the same entries. <code>O(n)</code>.
     *
     * @param m1         the first {@link Map}
     * @param m2         the second {@link Map}
     * @param valueEqRel the {@link EquivalenceRelation} to use to compare entry values
     * @param <K>        the key type
     * @param <V>        the value type
     * @param <M>        the {@link Map} subtype of the arguments
     * @return true if both {@link Map}s have the same entries by the parameters above; false otherwise
     */
    static <K, V, M extends Map<?, K, V>> boolean equals(M m1, M m2, EquivalenceRelation<V> valueEqRel) {
        return m1.sizeInfo().equals(m2.sizeInfo())
                && and().foldMap(into((k, v) -> m2.get(k).fmap(valueEqRel.apply(v)).orElse(false)), m1);
    }
}
