package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.internal.Bitmap32;
import com.jnape.palatable.shoki.internal.DenseArrays;

import java.util.Arrays;
import java.util.Map;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.internal.Bitmap32.bitmap32;
import static com.jnape.palatable.shoki.internal.Indices.bitmapIndex;

public final class ImmutableHashMap<K, V> implements RandomAccess<K, V>, Sizable {

    private static final ImmutableHashMap<?, ?> DEFAULT_EMPTY = empty(objectEquals(), objectHashCode());

    private final EquivalenceRelation<K> keyEquivalenceRelation;
    private final HashingAlgorithm<K>    keyHashingAlgorithm;
    private final Bitmap32               bitmap;
    private final Object[]               table;

    private ImmutableHashMap(EquivalenceRelation<K> keyEquivalenceRelation, HashingAlgorithm<K> keyHashingAlgorithm,
                             Bitmap32 bitmap, Object[] table) {
        this.keyEquivalenceRelation = keyEquivalenceRelation;
        this.keyHashingAlgorithm = keyHashingAlgorithm;
        this.bitmap = bitmap;
        this.table = table;
    }

    @Override
    public Maybe<V> get(K key) {
        return getForHashLevel(key, bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    public ImmutableHashMap<K, V> put(K key, V value) {
        return putForHashAndLevel(new Entry<>(key, value), bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    public ImmutableHashMap<K, V> remove(K key) {
        return removeForHashLevel(key, bitmap32(keyHashingAlgorithm.apply(key)), 1);
    }

    public boolean contains(K key) {
        return get(key).match(constantly(false), constantly(true));
    }

    @Override
    public SizeInfo.Known<Integer> sizeInfo() {
        return SizeInfo.known(Arrays.stream(table)
                                  .reduce(0, (size, obj) -> {
                                      if (obj instanceof ImmutableHashMap<?, ?>) {
                                          return size + ((ImmutableHashMap<?, ?>) obj).sizeInfo().getSize();
                                      } else if (obj instanceof Collision<?, ?>) {
                                          return size + ((Collision<?, ?>) obj).size();
                                      } else {
                                          return size + 1;
                                      }
                                  }, Integer::sum));
    }

    private Maybe<V> getForHashLevel(K key, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return Maybe.nothing();

        Object valueAtIndex = table[tableIndex(bitmapIndex)];
        if (valueAtIndex instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) valueAtIndex;
            return keyEquivalenceRelation.apply(key, entry.k) ? just(entry.v) : nothing();
        } else if (valueAtIndex instanceof ImmutableHashMap<?, ?>) {
            @SuppressWarnings("unchecked")
            ImmutableHashMap<K, V> subTrie = (ImmutableHashMap<K, V>) valueAtIndex;
            return subTrie.getForHashLevel(key, keyHash, level + 1);
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) valueAtIndex;
            return collision.get(key, keyHash, keyEquivalenceRelation);
        }
    }

    private ImmutableHashMap<K, V> putForHashAndLevel(Entry<K, V> entry, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return insertAt(bitmapIndex, entry);

        Object obj = table[tableIndex(bitmapIndex)];
        if (obj instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> existingEntry = (Entry<K, V>) obj;
            return keyEquivalenceRelation.apply(existingEntry.k, entry.k)
                ? overrideAt(bitmapIndex, entry)
                : propagateBoth(existingEntry, bitmap32(keyHashingAlgorithm.apply(existingEntry.k)),
                                entry, keyHash,
                                bitmapIndex, level);
        } else if (obj instanceof ImmutableHashMap<?, ?>) {
            @SuppressWarnings("unchecked")
            ImmutableHashMap<K, V> subTrie = (ImmutableHashMap<K, V>) obj;
            return overrideAt(bitmapIndex, subTrie.putForHashAndLevel(entry, keyHash, level + 1));
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) obj;
            return overrideAt(bitmapIndex, collision.put(entry));
        }
    }

    private ImmutableHashMap<K, V> propagateBoth(Entry<K, V> existingEntry, Bitmap32 existingKeyHash,
                                                 Entry<K, V> newEntry, Bitmap32 newKeyHash,
                                                 int index, int level) {
        int nextLevel = level + 1;
        return nextLevel == 8
            ? overrideAt(index, new Collision<>(existingKeyHash, ImmutableStack.of(existingEntry, newEntry)))
            : overrideAt(index, ImmutableHashMap.<K, V>empty(keyEquivalenceRelation, keyHashingAlgorithm)
            .putForHashAndLevel(existingEntry, existingKeyHash, nextLevel)
            .putForHashAndLevel(newEntry, newKeyHash, nextLevel));
    }

    private ImmutableHashMap<K, V> removeForHashLevel(K key, Bitmap32 keyHash, int level) {
        int bitmapIndex = bitmapIndex(keyHash, level);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return this;

        Object valueAtIndex = table[tableIndex(bitmapIndex)];
        if (valueAtIndex instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) valueAtIndex;
            return keyEquivalenceRelation.apply(key, entry.k) ? deleteAt(bitmapIndex) : this;
        } else if (valueAtIndex instanceof ImmutableHashMap<?, ?>) {
            @SuppressWarnings("unchecked")
            ImmutableHashMap<K, V> subTrie = (ImmutableHashMap<K, V>) valueAtIndex;
            return overrideAt(bitmapIndex, subTrie.removeForHashLevel(key, keyHash, level + 1));
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) valueAtIndex;
            return collision.removeAndUpdate(this, key, keyHash, bitmapIndex);
        }
    }

    private int tableIndex(int index) {
        return bitmap.lowerBits(index).populationCount();
    }

    private ImmutableHashMap<K, V> insertAt(int index, Object valueForSlot) {
        return new ImmutableHashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap.populateAtIndex(index),
                                      DenseArrays.insertAt(tableIndex(index), table, valueForSlot));
    }

    private ImmutableHashMap<K, V> overrideAt(int index, Object valueForSlot) {
        return new ImmutableHashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap,
                                      DenseArrays.overrideAt(tableIndex(index), table, valueForSlot));
    }

    private ImmutableHashMap<K, V> deleteAt(int index) {
        return new ImmutableHashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, bitmap.evictAtIndex(index),
                                      DenseArrays.deleteAt(tableIndex(index), table));
    }

    public static <K, V> ImmutableHashMap<K, V> empty(EquivalenceRelation<K> equivalenceRelation,
                                                      HashingAlgorithm<K> hashingAlgorithm) {
        return new ImmutableHashMap<>(equivalenceRelation, hashingAlgorithm, Bitmap32.empty(), new Object[0]);
    }

    @SuppressWarnings("unchecked")
    public static <K, V> ImmutableHashMap<K, V> empty() {
        return (ImmutableHashMap<K, V>) DEFAULT_EMPTY;
    }

    public static <K, V> ImmutableHashMap<K, V> fromJavaMap(Map<K, V> map) {
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

        public Maybe<V> get(K k, Bitmap32 keyHash, EquivalenceRelation<K> keyEquivalence) {
            if (keyHash.equals(this.keyHash))
                for (Entry<K, V> kvPair : kvPairs)
                    if (keyEquivalence.apply(kvPair.k, k))
                        return just(kvPair.v);

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
                                                       Bitmap32 keyHash, int index) {
            if (size() == 1) {
                return immutableHashMap.deleteAt(index);
            }
            Collision<K, V> withoutKey = remove(key, keyHash, immutableHashMap.keyEquivalenceRelation);
            return withoutKey.size() == 1
                ? immutableHashMap.overrideAt(index, withoutKey.kvPairs.iterator().next())
                : immutableHashMap.overrideAt(index, withoutKey);
        }
    }

}
