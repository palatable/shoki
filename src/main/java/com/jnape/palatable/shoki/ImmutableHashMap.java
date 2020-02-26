package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.lambda.functions.builtin.fn1.Head;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.Map;
import com.jnape.palatable.shoki.internal.Arrays;
import com.jnape.palatable.shoki.internal.Bitmap32;

import java.util.Iterator;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Flatten.flatten;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.internal.Bitmap32.bitmap32;
import static com.jnape.palatable.shoki.internal.Indices.bitmapIndex;
import static com.jnape.palatable.shoki.internal.Indices.tableIndex;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;

public final class ImmutableHashMap<K, V> implements Map<Integer, K, V> {

    private static final ImmutableHashMap<?, ?> DEFAULT_EMPTY = empty(objectEquals(), objectHashCode());

    private final EquivalenceRelation<K> keyEquivalenceRelation;
    private final HashingAlgorithm<K>    keyHashingAlgorithm;

    private final Bitmap32 bitmap;
    private final Object[] table;

    private ImmutableHashMap(EquivalenceRelation<K> keyEquivalenceRelation, HashingAlgorithm<K> keyHashingAlgorithm,
                             Bitmap32 bitmap, Object[] table) {
        this.keyEquivalenceRelation = keyEquivalenceRelation;
        this.keyHashingAlgorithm = keyHashingAlgorithm;
        this.bitmap = bitmap;
        this.table = table;
    }

    @Override
    public boolean contains(K key) {
        return get(key).match(constantly(false), constantly(true));
    }

    @Override
    public Maybe<V> get(K key) {
        return getEntry(key).fmap(e -> e.v);
    }

    @Override
    public ImmutableHashMap<K, V> put(K key, V value) {
        return putForHashAndLevel(new Entry<>(key, value), bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    @Override
    public ImmutableHashMap<K, V> remove(K key) {
        return removeForHashLevel(key, bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    @Override
    public boolean isEmpty() {
        return bitmap.populationCount() == 0;
    }

    @Override
    public Maybe<Tuple2<K, V>> head() {
        return Head.head(this);
    }

    @Override
    public ImmutableHashMap<K, V> tail() {
        return head()
            .fmap(into((headKey, __) -> remove(headKey)))
            .orElse(this);
    }

    @Override
    public String toString() {
        return "ImmutableHashMap["
            + join("|", map(into((k, v) -> format("(%s=%s)", k, v)), this))
            + "]";
    }

    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        Iterable<Iterable<Tuple2<K, V>>> map = map(valueAtSlot -> match(
            valueAtSlot,
            entry -> singletonList(tuple(entry.k, entry.v)),
            subTrie -> subTrie,
            collision -> map(entry -> tuple(entry.k, entry.v), collision.kvPairs)), asList(table));
        return flatten(map).iterator();
    }

    @Override
    public SizeInfo.Known<Integer> sizeInfo() {
        return SizeInfo.known(java.util.Arrays.stream(table)
                                  .reduce(0, (size, obj) -> match(
                                      obj,
                                      entry -> size + 1,
                                      map -> size + map.sizeInfo().getSize(),
                                      collision -> size + collision.size()), Integer::sum));
    }

    private Maybe<Entry<K, V>> getEntry(K key) {
        return getEntryForHashLevel(key, bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    public ImmutableHashSet<K> keys() {
        return foldLeft((keys, kv) -> keys.add(kv._1()), ImmutableHashSet.empty(), this);
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean equals(Object other) {
        try {
            return other instanceof ImmutableHashMap<?, ?>
                && sameEntries((ImmutableHashMap<K, V>) other);
        } catch (ClassCastException cce) {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return foldLeft(Integer::sum, 0,
                        map(into((k, v) -> 31 * keyHashingAlgorithm.apply(k) + Objects.hashCode(v)), this));
    }

    public boolean sameEntries(ImmutableHashMap<K, V> other) {
        return sameEntries(other, objectEquals());
    }

    public boolean sameEntries(ImmutableHashMap<K, V> other, EquivalenceRelation<V> valueEquivalenceRelation) {
        if (!sizeInfo().equals(other.sizeInfo())) {
            return false;
        }

        for (Tuple2<K, V> entry : this) {
            K myKey = entry.getKey();
            if (!other.getEntry(myKey).match(
                constantly(false),
                otherEntry -> keyEquivalenceRelation.apply(otherEntry.k, myKey)
                    && valueEquivalenceRelation.apply(otherEntry.v, entry.getValue())))
                return false;
        }

        return true;
    }

    private <R> R match(Object valueAtIndex,
                        Fn1<? super Entry<K, V>, ? extends R> entryFn,
                        Fn1<? super ImmutableHashMap<K, V>, ? extends R> mapFn,
                        Fn1<? super Collision<K, V>, ? extends R> collisionFn) {
        if (valueAtIndex instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) valueAtIndex;
            return entryFn.apply(entry);
        } else if (valueAtIndex instanceof ImmutableHashMap<?, ?>) {
            @SuppressWarnings("unchecked")
            ImmutableHashMap<K, V> subTrie = (ImmutableHashMap<K, V>) valueAtIndex;
            return mapFn.apply(subTrie);
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) valueAtIndex;
            return collisionFn.apply(collision);
        }
    }

    private Maybe<Entry<K, V>> getEntryForHashLevel(K key, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        return bitmap.populatedAtIndex(bitmapIndex)
            ? match(table[tableIndex(bitmap, bitmapIndex)],
                    entry -> keyEquivalenceRelation.apply(key, entry.k) ? just(entry) : nothing(),
                    subTrie -> subTrie.getEntryForHashLevel(key, keyHash, level + 1),
                    collision -> collision.get(key, keyHash, keyEquivalenceRelation))
            : Maybe.nothing();
    }

    private ImmutableHashMap<K, V> putForHashAndLevel(Entry<K, V> entry, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        int tableIndex  = tableIndex(bitmap, bitmapIndex);
        return bitmap.populatedAtIndex(bitmapIndex)
            ? match(table[tableIndex],
                    existingEntry -> keyEquivalenceRelation.apply(existingEntry.k, entry.k)
                        ? overrideAt(entry, tableIndex)
                        : propagateBoth(existingEntry,
                                        bitmap32(keyHashingAlgorithm.apply(existingEntry.k)),
                                        entry, keyHash,
                                        level, tableIndex),
                    subTrie -> overrideAt(subTrie.putForHashAndLevel(entry, keyHash, level + 1), tableIndex),
                    collision -> overrideAt(collision.put(entry), tableIndex))
            : insertAt(bitmapIndex, entry, tableIndex);
    }

    private ImmutableHashMap<K, V> propagateBoth(Entry<K, V> existingEntry, Bitmap32 existingKeyHash,
                                                 Entry<K, V> newEntry, Bitmap32 newKeyHash,
                                                 int level, int tableIndex) {
        int nextLevel = level + 1;
        return nextLevel == 8
            ? overrideAt(new Collision<>(existingKeyHash, ImmutableStack.of(existingEntry, newEntry)), tableIndex)
            : overrideAt(ImmutableHashMap.<K, V>empty(keyEquivalenceRelation, keyHashingAlgorithm)
                             .putForHashAndLevel(existingEntry, existingKeyHash, nextLevel)
                             .putForHashAndLevel(newEntry, newKeyHash, nextLevel),
                         tableIndex);
    }

    private ImmutableHashMap<K, V> removeForHashLevel(K key, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return this;

        int tableIndex = tableIndex(bitmap, bitmapIndex);
        return match(table[tableIndex],
                     entry -> keyEquivalenceRelation.apply(key, entry.k) ? deleteAt(bitmapIndex, tableIndex) : this,
                     subTrie -> overrideAt(subTrie.removeForHashLevel(key, keyHash, level + 1), tableIndex),
                     collision -> collision.removeAndUpdate(this, key, keyHash, bitmapIndex, tableIndex));
    }

    private ImmutableHashMap<K, V> insertAt(int index, Object valueForSlot, int tableIndex) {
        return new ImmutableHashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap.populateAtIndex(index),
                                      Arrays.insertAt(tableIndex, table, valueForSlot));
    }

    private ImmutableHashMap<K, V> overrideAt(Object valueForSlot, int tableIndex) {
        return new ImmutableHashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap,
                                      Arrays.overrideAt(tableIndex, table, valueForSlot));
    }

    private ImmutableHashMap<K, V> deleteAt(int index, int tableIndex) {
        return new ImmutableHashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap.evictAtIndex(index),
                                      Arrays.deleteAt(tableIndex, table));
    }

    public static <K, V> ImmutableHashMap<K, V> empty(EquivalenceRelation<K> equivalenceRelation,
                                                      HashingAlgorithm<K> hashingAlgorithm) {
        return new ImmutableHashMap<>(equivalenceRelation, hashingAlgorithm, Bitmap32.empty(), new Object[0]);
    }

    @SuppressWarnings("unchecked")
    public static <K, V> ImmutableHashMap<K, V> empty() {
        return (ImmutableHashMap<K, V>) DEFAULT_EMPTY;
    }

    public static <K, V> ImmutableHashMap<K, V> fromJavaMap(java.util.Map<K, V> map) {
        return foldLeft((immutableHashMap, entry) -> immutableHashMap.put(entry.getKey(), entry.getValue()),
                        empty(),
                        map.entrySet());
    }

    private static final class Entry<K, V> {

        private final K k;
        private final V v;

        private Entry(K k, V v) {
            this.k = k;
            this.v = v;
        }
    }

    private static final class Collision<K, V> {
        private final Bitmap32                    keyHash;
        private final ImmutableStack<Entry<K, V>> kvPairs;

        private Collision(Bitmap32 keyHash,
                          ImmutableStack<Entry<K, V>> kvPairs) {
            this.keyHash = keyHash;
            this.kvPairs = kvPairs;
        }

        public Maybe<Entry<K, V>> get(K k, Bitmap32 keyHash, EquivalenceRelation<K> keyEquivalence) {
            if (keyHash.equals(this.keyHash))
                for (Entry<K, V> kvPair : kvPairs)
                    if (keyEquivalence.apply(kvPair.k, k))
                        return just(kvPair);

            return nothing();
        }

        public Collision<K, V> put(Entry<K, V> entry) {
            return new Collision<>(keyHash, kvPairs.cons(entry));
        }

        public Collision<K, V> remove(K k, Bitmap32 keyHash, EquivalenceRelation<K> keyEquivalence) {
            return !keyHash.equals(this.keyHash)
                ? this
                : new Collision<>(keyHash, foldLeft(((s, kv) -> !keyEquivalence.apply(k, kv.k) ? s.cons(kv) : s),
                                                    ImmutableStack.empty(),
                                                    kvPairs));
        }

        public Integer size() {
            return kvPairs.sizeInfo().getSize();
        }

        private ImmutableHashMap<K, V> removeAndUpdate(ImmutableHashMap<K, V> immutableHashMap, K key,
                                                       Bitmap32 keyHash, int index, int tableIndex) {
            if (size() == 1) {
                return immutableHashMap.deleteAt(index, tableIndex);
            }
            Collision<K, V> withoutKey = remove(key, keyHash, immutableHashMap.keyEquivalenceRelation);
            return withoutKey.size() == 1
                ? immutableHashMap.overrideAt(withoutKey.kvPairs.iterator().next(), tableIndex)
                : immutableHashMap.overrideAt(withoutKey, tableIndex);
        }
    }

}
