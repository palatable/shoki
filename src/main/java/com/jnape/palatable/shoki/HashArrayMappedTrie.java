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
import static com.jnape.palatable.lambda.functions.builtin.fn1.Id.id;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Size.size;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.internal.Bitmap32.bitmap32;
import static com.jnape.palatable.shoki.internal.Indices.bitmapIndex;
import static com.jnape.palatable.shoki.internal.Indices.tableIndex;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Arrays.asList;
import static java.util.Collections.singleton;

public final class HashArrayMappedTrie<K, V> implements Map<Integer, K, V> {

    private static final HashArrayMappedTrie<?, ?> DEFAULT_EMPTY = empty(objectEquals(), objectHashCode());

    private final EquivalenceRelation<K> keyEquivalenceRelation;
    private final HashingAlgorithm<K>    keyHashingAlgorithm;

    private final Bitmap32 bitmap;
    private final Object[] table;

    private HashArrayMappedTrie(EquivalenceRelation<K> keyEquivalenceRelation, HashingAlgorithm<K> keyHashingAlgorithm,
                                Bitmap32 bitmap, Object[] table) {
        this.keyEquivalenceRelation = keyEquivalenceRelation;
        this.keyHashingAlgorithm = keyHashingAlgorithm;
        this.bitmap = bitmap;
        this.table = table;
    }

    @Override
    public HashArrayMappedTrie<K, V> put(K key, V value) {
        return putForHashAndLevel(new Entry<>(key, value), bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    @Override
    public HashArrayMappedTrie<K, V> remove(K key) {
        return removeForHashLevel(key, bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    @Override
    public ImmutableHashSet<K> keys() {
        return foldLeft((keys, kv) -> keys.add(kv._1()), ImmutableHashSet.empty(), this);
    }

    @Override
    public HashArrayMappedTrie<K, V> tail() {
        return head()
                .fmap(into((headKey, __) -> remove(headKey)))
                .orElse(this);
    }

    @Override
    public Maybe<V> get(K key) {
        return getEntry(key).fmap(e -> e.v);
    }

    @Override
    public boolean contains(K key) {
        return get(key).match(constantly(false), constantly(true));
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
    public String toString() {
        return "ImmutableHashMap["
                + join(", ", map(into((k, v) -> format("(%s=%s)", k, v)), this))
                + "]";
    }

    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        Iterable<Iterable<Tuple2<K, V>>> map = map(valueAtSlot -> match(
                valueAtSlot,
                entry -> singleton(tuple(entry.k, entry.v)),
                id(),
                collision -> map(entry -> tuple(entry.k, entry.v), collision.kvPairs)), asList(table));
        return flatten(map).iterator();
    }

    @Override
    public SizeInfo.Known<Integer> sizeInfo() {
        return known(size(this).intValue());
    }

    private Maybe<Entry<K, V>> getEntry(K key) {
        return getEntryForHashLevel(key, bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean equals(Object other) {
        try {
            return other instanceof HashArrayMappedTrie<?, ?>
                    && sameEntries((HashArrayMappedTrie<K, V>) other);
        } catch (ClassCastException cce) {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return foldLeft(Integer::sum, 0,
                        map(into((k, v) -> 31 * keyHashingAlgorithm.apply(k) + Objects.hashCode(v)), this));
    }

    public boolean sameEntries(HashArrayMappedTrie<K, V> other) {
        return sameEntries(other, objectEquals());
    }

    public boolean sameEntries(HashArrayMappedTrie<K, V> other, EquivalenceRelation<V> valueEquivalenceRelation) {
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

    private Maybe<Entry<K, V>> getEntryForHashLevel(K key, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        return bitmap.populatedAtIndex(bitmapIndex)
               ? match(table[tableIndex(bitmap, bitmapIndex)],
                       entry -> keyEquivalenceRelation.apply(key, entry.k) ? just(entry) : nothing(),
                       subTrie -> subTrie.getEntryForHashLevel(key, keyHash, level + 1),
                       collision -> collision.get(key, keyHash, keyEquivalenceRelation))
               : Maybe.nothing();
    }

    private HashArrayMappedTrie<K, V> putForHashAndLevel(Entry<K, V> entry, Bitmap32 keyHash, int level) {
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

    private HashArrayMappedTrie<K, V> propagateBoth(Entry<K, V> existingEntry, Bitmap32 existingKeyHash,
                                                    Entry<K, V> newEntry, Bitmap32 newKeyHash,
                                                    int level, int tableIndex) {
        int nextLevel = level + 1;
        return nextLevel == 8
               ? overrideAt(new Collision<>(existingKeyHash, ImmutableStack.of(existingEntry, newEntry)), tableIndex)
               : overrideAt(HashArrayMappedTrie.<K, V>empty(keyEquivalenceRelation, keyHashingAlgorithm)
                                    .putForHashAndLevel(existingEntry, existingKeyHash, nextLevel)
                                    .putForHashAndLevel(newEntry, newKeyHash, nextLevel),
                            tableIndex);
    }

    private HashArrayMappedTrie<K, V> removeForHashLevel(K key, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return this;

        int tableIndex = tableIndex(bitmap, bitmapIndex);
        return match(table[tableIndex],
                     entry -> keyEquivalenceRelation.apply(key, entry.k) ? deleteAt(bitmapIndex, tableIndex) : this,
                     subTrie -> overrideAt(subTrie.removeForHashLevel(key, keyHash, level + 1), tableIndex),
                     collision -> collision.removeAndUpdate(this, key, keyHash, bitmapIndex, tableIndex));
    }

    private HashArrayMappedTrie<K, V> insertAt(int index, Object valueForSlot, int tableIndex) {
        return new HashArrayMappedTrie<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap.populateAtIndex(index),
                                         Arrays.insertAt(tableIndex, table, valueForSlot));
    }

    private HashArrayMappedTrie<K, V> overrideAt(Object valueForSlot, int tableIndex) {
        return new HashArrayMappedTrie<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap,
                                         Arrays.overrideAt(tableIndex, table, valueForSlot));
    }

    private HashArrayMappedTrie<K, V> deleteAt(int index, int tableIndex) {
        return new HashArrayMappedTrie<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap.evictAtIndex(index),
                                         Arrays.deleteAt(tableIndex, table));
    }

    private <R> R match(Object valueAtIndex,
                        Fn1<? super Entry<K, V>, ? extends R> entryFn,
                        Fn1<? super HashArrayMappedTrie<K, V>, ? extends R> mapFn,
                        Fn1<? super Collision<K, V>, ? extends R> collisionFn) {
        if (valueAtIndex instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) valueAtIndex;
            return entryFn.apply(entry);
        } else if (valueAtIndex instanceof HashArrayMappedTrie<?, ?>) {
            @SuppressWarnings("unchecked")
            HashArrayMappedTrie<K, V> subTrie = (HashArrayMappedTrie<K, V>) valueAtIndex;
            return mapFn.apply(subTrie);
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) valueAtIndex;
            return collisionFn.apply(collision);
        }
    }

    public static <K, V> HashArrayMappedTrie<K, V> empty(EquivalenceRelation<K> equivalenceRelation,
                                                         HashingAlgorithm<K> hashingAlgorithm) {
        return new HashArrayMappedTrie<>(equivalenceRelation, hashingAlgorithm, Bitmap32.empty(), new Object[0]);
    }

    @SuppressWarnings("unchecked")
    public static <K, V> HashArrayMappedTrie<K, V> empty() {
        return (HashArrayMappedTrie<K, V>) DEFAULT_EMPTY;
    }

    @SafeVarargs
    public static <K, V> HashArrayMappedTrie<K, V> hashArrayMappedTrie(EquivalenceRelation<K> keyEquivalenceRelation,
                                                                       HashingAlgorithm<K> keyHashingAlgorithm,
                                                                       Tuple2<K, V> entry, Tuple2<K, V>... entries) {
        return foldLeft((m, e) -> m.put(e._1(), e._2()),
                        HashArrayMappedTrie.<K, V>empty(keyEquivalenceRelation, keyHashingAlgorithm)
                                .put(entry._1(), entry._2()),
                        asList(entries));
    }

    @SafeVarargs
    public static <K, V> HashArrayMappedTrie<K, V> hashArrayMappedTrie(Tuple2<K, V> entry, Tuple2<K, V>... entries) {
        return hashArrayMappedTrie(objectEquals(), objectHashCode(), entry, entries);
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

        private HashArrayMappedTrie<K, V> removeAndUpdate(HashArrayMappedTrie<K, V> hashArrayMappedTrie, K key,
                                                          Bitmap32 keyHash, int index, int tableIndex) {
            if (size() == 1) {
                return hashArrayMappedTrie.deleteAt(index, tableIndex);
            }
            Collision<K, V> withoutKey = remove(key, keyHash, hashArrayMappedTrie.keyEquivalenceRelation);
            return withoutKey.size() == 1
                   ? hashArrayMappedTrie.overrideAt(withoutKey.kvPairs.iterator().next(), tableIndex)
                   : hashArrayMappedTrie.overrideAt(withoutKey, tableIndex);
        }
    }
}
