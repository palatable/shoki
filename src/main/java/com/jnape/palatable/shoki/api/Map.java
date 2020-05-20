package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.semigroup.Semigroup;

import static com.jnape.palatable.lambda.functions.Fn2.curried;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.monoid.builtin.And.and;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;

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
     * If <code>key</code> is associated to a value inside this {@link Map}, retrieve {@link Maybe#just(Object) just}
     * the value it maps to; otherwise, return {@link Maybe#nothing() nothing}.
     *
     * @see Map#put(Object, Object)
     * @see Map#remove(Object)
     */
    @Override
    Maybe<V> get(K k);

    /**
     * {@inheritDoc}
     * <code>True</code> if <code>key</code> is associated to a value in this {@link Map}; <code>false</code> otherwise.
     */
    @Override
    default boolean contains(K k) {
        return get(k).match(constantly(false), constantly(true));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default boolean isEmpty() {
        return Collection.super.isEmpty();
    }

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
     * {@link Map#put(Object, Object) Put} each entry in <code>map</code> in this {@link Map}, relying on
     * <code>semigroup</code> to consolidate colliding values in the case of duplicate keys.
     *
     * @param other     the other {@link Map}
     * @param semigroup the {@link Semigroup}
     * @return the merged {@link Map}
     */
    default Map<Size, K, V> merge(Map<Size, K, V> other, Semigroup<V> semigroup) {
        return foldLeft(curried(m -> into((k, v) -> m.put(k, m.get(k).fmap(semigroup.flip().apply(v)).orElse(v)))),
                        this,
                        other);
    }

    /**
     * {@link Map#remove(Object) Remove} every key {@link Map#contains(Object) contained} in <code>keys</code> from
     * this {@link Map}.
     *
     * @param keys the {@link Set} of keys to remove from this {@link Map}
     * @return the updated {@link Map}
     */
    default Map<Size, K, V> removeAll(Set<Size, K> keys) {
        return foldLeft(Map<Size, K, V>::remove, this, keys);
    }

    /**
     * Common {@link EquivalenceRelation}s between {@link Map}s.
     */
    final class EquivalenceRelations {
        private EquivalenceRelations() {
        }

        /**
         * An {@link EquivalenceRelation} between two {@link Map}s that holds if, and only if, both {@link Map}s have
         * equivalent {@link SizeInfo}s and contain the same entries.
         *
         * @param valueEqRel the {@link EquivalenceRelation} to use to compare entry values
         * @param <K>        the key type
         * @param <V>        the value type
         * @param <M>        the {@link Map} subtype of the arguments
         * @return the {@link EquivalenceRelation}
         */
        public static <K, V, M extends Map<?, K, V>> EquivalenceRelation<M> entries(
                EquivalenceRelation<? super V> valueEqRel) {
            return Sizable.EquivalenceRelations.<M>sizeInfos()
                    .and((m1, m2) -> and()
                            .foldMap(into((k, v) -> m2.get(k).fmap(valueEqRel.apply(v)).orElse(false)), m1));
        }

        /**
         * An {@link EquivalenceRelation} between two {@link Map}s that holds if, and only if, both {@link Map}s have
         * equivalent {@link SizeInfo}s and {@link Map#keys() key sets}.
         *
         * @param keySetEqRel the {@link EquivalenceRelation} to use to compare key sets
         * @param <K>         the key type
         * @param <V>         the value type
         * @param <M>         the {@link Map} subtype of the arguments
         * @return the {@link EquivalenceRelation}
         */
        public static <K, V, M extends Map<?, K, V>> EquivalenceRelation<M> keys(
                EquivalenceRelation<? super Set<?, K>> keySetEqRel) {
            return Sizable.EquivalenceRelations.<M>sizeInfos()
                    .and((m1, m2) -> equivalent(keySetEqRel, m1.keys(), m2.keys()));
        }
    }

    /**
     * Common {@link HashingAlgorithm}s for {@link Map}s.
     */
    final class HashingAlgorithms {
        private HashingAlgorithms() {
        }

        /**
         * A {@link HashingAlgorithm} derived from the key/value pairs contained in any order in a given {@link Map}.
         *
         * @param keyHashingAlgorithm   the key {@link HashingAlgorithm}
         * @param valueHashingAlgorithm the value {@link HashingAlgorithm}
         * @param <K>                   the key type
         * @param <V>                   the value type
         * @param <M>                   the  {@link Map} subtype of the argument
         * @return the {@link HashingAlgorithm}
         */
        public static <K, V, M extends Map<?, K, V>> HashingAlgorithm<M> entries(
                HashingAlgorithm<? super K> keyHashingAlgorithm,
                HashingAlgorithm<? super V> valueHashingAlgorithm) {
            return m -> foldLeft(Integer::sum, 0,
                                 map(into((K k, V v) -> keyHashingAlgorithm.apply(k)
                                         ^ valueHashingAlgorithm.apply(v)), m));
        }
    }
}
