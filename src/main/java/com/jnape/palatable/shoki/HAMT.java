package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.Bitmap32.bitmap32;
import static com.jnape.palatable.shoki.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.HashingAlgorithm.objectHashCode;
import static java.lang.System.arraycopy;

public final class HAMT<K, V> implements RandomAccess<K, V>, Sizable {

    private static final HAMT<?, ?> DEFAULT_EMPTY = empty(objectEquals(), objectHashCode());

    private final EquivalenceRelation<K> keyEquivalence;
    private final HashingAlgorithm<K>    hashingAlgorithm;
    private final Bitmap32               bitmap;
    private final Object[]               table;

    private HAMT(EquivalenceRelation<K> keyEquivalence, HashingAlgorithm<K> hashingAlgorithm,
                 Bitmap32 bitmap, Object[] table) {
        this.keyEquivalence = keyEquivalence;
        this.hashingAlgorithm = hashingAlgorithm;
        this.bitmap = bitmap;
        this.table = table;
    }

    @Override
    public Maybe<V> get(K key) {
        return getForHashLevel(key, bitmap32(hashingAlgorithm.apply(key)), 1);
    }

    public HAMT<K, V> put(K key, V value) {
        return putForHashLevel(new Entry<>(bitmap32(hashingAlgorithm.apply(key)), key, value), 1);
    }

    public HAMT<K, V> remove(K key) {
        return removeForHashLevel(key, bitmap32(hashingAlgorithm.apply(key)), 1);
    }

    public boolean contains(K key) {
        return get(key).match(constantly(false), constantly(true));
    }

    private Maybe<V> getForHashLevel(K key, Bitmap32 keyHash, int level) {
        int index = keyHash.index(level);
        if (!bitmap.populatedAtIndex(index))
            return Maybe.nothing();

        Object valueAtIndex = table[index];
        if (valueAtIndex instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) valueAtIndex;
            return keyEquivalence.apply(key, entry.k) ? just(entry.v) : nothing();
        } else if (valueAtIndex instanceof HAMT<?, ?>) {
            @SuppressWarnings("unchecked")
            HAMT<K, V> subTrie = (HAMT<K, V>) valueAtIndex;
            return subTrie.getForHashLevel(key, keyHash, level + 1);
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) valueAtIndex;
            return collision.get(key, keyHash, keyEquivalence);
        }
    }

    private HAMT<K, V> putForHashLevel(Entry<K, V> entry, int level) {
        int index = entry.keyHash.index(level);
        if (!bitmap.populatedAtIndex(index))
            return insertEntry(entry, index);

        return replaceOrPropagate(entry, index, level);
    }

    private HAMT<K, V> insertEntry(Entry<K, V> entry, int index) {
        return insert(entry, index);
    }

    private HAMT<K, V> replaceOrPropagate(Entry<K, V> newEntry, int index, int level) {
        Object obj = table[index];
        if (obj instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> existingEntry = (Entry<K, V>) obj;
            return keyEquivalence.apply(existingEntry.k, newEntry.k)
                   ? insertEntry(newEntry, index)
                   : propagateBoth(existingEntry, newEntry, index, level, existingEntry.keyHash);
        } else if (obj instanceof HAMT<?, ?>) {
            @SuppressWarnings("unchecked")
            HAMT<K, V> subTrie = (HAMT<K, V>) obj;
            return insert(subTrie.putForHashLevel(newEntry, level + 1), index);
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) obj;
            return insert(collision.put(newEntry.k, newEntry.v), index);
        }
    }

    private HAMT<K, V> propagateBoth(Entry<K, V> existingEntry, Entry<K, V> newEntry, int index, int level,
                                     Bitmap32 existingKeyHash) {
        int nextLevel = level + 1;

        if (nextLevel == 8) {
            return insert(new Collision<>(existingKeyHash,
                                          ImmutableStack.of(tuple(existingEntry.k, existingEntry.v),
                                                            tuple(newEntry.k, newEntry.v))),
                          index);
        } else {
            HAMT<K, V> subTrie = HAMT.<K, V>empty()
                    .putForHashLevel(existingEntry, nextLevel)
                    .putForHashLevel(newEntry, nextLevel);
            return insert(subTrie, index);
        }
    }

    private HAMT<K, V> insert(Object valueForSlot, int index) {
        return new HAMT<>(keyEquivalence, hashingAlgorithm, bitmap.populateAtIndex(index),
                          writeValueAtIndex(valueForSlot, index));
    }

    private Object[] writeValueAtIndex(Object valueForSlot, int index) {
        Object[] copy = new Object[32];
        if (index == 0) {
            arraycopy(table, 1, copy, 1, 31);
            copy[0] = valueForSlot;
        } else if (index == 31) {
            arraycopy(table, 0, copy, 0, 31);
            copy[31] = valueForSlot;
        } else {
            arraycopy(table, 0, copy, 0, index);
            copy[index] = valueForSlot;
            arraycopy(table, index + 1, copy, index + 1, 31 - index);
        }
        return copy;
    }


    private HAMT<K, V> removeForHashLevel(K key, Bitmap32 keyHash, int level) {
        int index = keyHash.index(level);
        if (!bitmap.populatedAtIndex(index))
            return this;

        Object valueAtIndex = table[index];
        if (valueAtIndex instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) valueAtIndex;
            return keyEquivalence.apply(key, entry.k) ? delete(index) : this;
        } else if (valueAtIndex instanceof HAMT<?, ?>) {
            @SuppressWarnings("unchecked")
            HAMT<K, V> subTrie = (HAMT<K, V>) valueAtIndex;
            return insert(subTrie.removeForHashLevel(key, keyHash, level + 1), index);
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) valueAtIndex;
            Collision<K, V> afterRemove = collision.remove(key, keyHash, keyEquivalence);
            return afterRemove.kvPairs.isEmpty()
                   ? delete(index)
                   : insert(afterRemove, index);
        }
    }

    private HAMT<K, V> delete(int index) {
        return new HAMT<>(keyEquivalence, hashingAlgorithm, bitmap.evictAtIndex(index), writeValueAtIndex(null, index));
    }

    @Override
    public SizeInfo.Known<Integer> sizeInfo() {
        return SizeInfo.known(Arrays.stream(table)
                                      .filter(Objects::nonNull)
                                      .reduce(0, (size, obj) -> {
                                          if (obj instanceof HAMT<?, ?>) {
                                              return size + ((HAMT<?, ?>) obj).sizeInfo().getSize();
                                          } else if (obj instanceof Collision<?, ?>) {
                                              return size + ((Collision<?, ?>) obj).size();
                                          } else {
                                              return size + 1;
                                          }
                                      }, Integer::sum));
    }

    public static <K, V> HAMT<K, V> empty(EquivalenceRelation<K> equivalenceRelation,
                                          HashingAlgorithm<K> hashingAlgorithm) {
        return new HAMT<>(equivalenceRelation, hashingAlgorithm, Bitmap32.empty(), new Object[32]);
    }

    @SuppressWarnings("unchecked")
    public static <K, V> HAMT<K, V> empty() {
        return (HAMT<K, V>) DEFAULT_EMPTY;
    }

    public static <K, V> HAMT<K, V> fromJavaMap(Map<K, V> map) {
        return foldLeft((hamt, entry) -> hamt.put(entry.getKey(), entry.getValue()),
                        empty(),
                        map.entrySet());
    }

    private static final class Entry<K, V> {

        private final Bitmap32 keyHash;
        private final K        k;
        private final V        v;

        private Entry(Bitmap32 keyHash, K k, V v) {
            this.keyHash = keyHash;
            this.k = k;
            this.v = v;
        }
    }

    private static final class Collision<K, V> {
        private final Bitmap32                     keyHash;
        private final ImmutableStack<Tuple2<K, V>> kvPairs;

        private Collision(Bitmap32 keyHash,
                          ImmutableStack<Tuple2<K, V>> kvPairs) {
            this.keyHash = keyHash;
            this.kvPairs = kvPairs;
        }

        public Maybe<V> get(K k, Bitmap32 keyHash, EquivalenceRelation<K> keyEquivalence) {
            if (keyHash.equals(this.keyHash))
                for (Tuple2<K, V> kvPair : kvPairs)
                    if (keyEquivalence.apply(kvPair._1(), k))
                        return just(kvPair._2());

            return nothing();
        }

        public Collision<K, V> put(K k, V v) {
            return new Collision<>(keyHash, kvPairs.cons(tuple(k, v)));
        }

        public Collision<K, V> remove(K k, Bitmap32 keyHash, EquivalenceRelation<K> keyEquivalence) {
            return !keyHash.equals(this.keyHash)
                   ? this
                   : new Collision<>(keyHash, foldLeft(((s, kv) -> !keyEquivalence.apply(k, kv._1()) ? s.cons(kv) : s),
                                                       ImmutableStack.empty(),
                                                       kvPairs));
        }

        public Integer size() {
            return kvPairs.sizeInfo().getSize();
        }
    }

}
