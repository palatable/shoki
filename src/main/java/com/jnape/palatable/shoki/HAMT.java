package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static java.lang.System.arraycopy;

public final class HAMT<K, V> {

    private final Bitmap32 bitmap;
    private final Object[] table;

    private HAMT(Bitmap32 bitmap, Object[] table) {
        this.bitmap = bitmap;
        this.table = table;
    }

    public Maybe<V> get(K k) {
        return getForHashLevel(k, Bitmap32.hash(k), 1);
    }

    public HAMT<K, V> put(K k, V v) {
        return putForHashLevel(new Entry<>(Bitmap32.hash(k), k, v), 1);
    }

    public boolean contains(K k) {
        return get(k).match(constantly(false), constantly(true));
    }

    private Maybe<V> getForHashLevel(K k, Bitmap32 keyHash, int level) {
        int index = keyHash.index(level);
        if (!bitmap.populatedAtIndex(index))
            return Maybe.nothing();

        Object valueAtIndex = table[index];
        if (valueAtIndex instanceof HAMT.Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) valueAtIndex;
            return Objects.equals(k, entry.k) ? just(entry.v) : nothing();
        } else if (valueAtIndex instanceof HAMT<?, ?>) {
            @SuppressWarnings("unchecked")
            HAMT<K, V> subTrie = (HAMT<K, V>) valueAtIndex;
            return subTrie.getForHashLevel(k, keyHash, level + 1);
        } else {
            @SuppressWarnings("unchecked")
            Collision<K, V> collision = (Collision<K, V>) valueAtIndex;
            return collision.get(k, keyHash);
        }
    }

    private HAMT<K, V> putForHashLevel(Entry<K, V> entry, int level) {
        int index = entry.keyHash.index(level);
        if (!bitmap.populatedAtIndex(index))
            return insert(entry, index);

        return replaceOrPropagate(entry, index, level);
    }

    private HAMT<K, V> replaceOrPropagate(Entry<K, V> newEntry, int index, int level) {
        Object obj = table[index];
        if (obj instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> existingEntry = (Entry<K, V>) obj;
            return Objects.equals(existingEntry.k, newEntry.k)
                   ? insert(newEntry, index)
                   : propagateBoth(existingEntry, newEntry, index, level);
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

    private HAMT<K, V> propagateBoth(Entry<K, V> existingEntry, Entry<K, V> newEntry, int index, int level) {
        int nextLevel = level + 1;

        if (nextLevel == 8) {
            return insert(new Collision<>(existingEntry.keyHash,
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

        return new HAMT<>(bitmap.populateAtIndex(index), copy);
    }

    public static <K, V> HAMT<K, V> empty() {
        return new HAMT<>(Bitmap32.empty(), new Object[32]);
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

        public Maybe<V> get(K k, Bitmap32 keyHash) {
            if (keyHash.equals(this.keyHash))
                for (Tuple2<K, V> kvPair : kvPairs)
                    if (Objects.equals(kvPair._1(), k))
                        return just(kvPair._2());

            return nothing();
        }

        public Collision<K, V> put(K k, V v) {
            return new Collision<>(keyHash, kvPairs.cons(tuple(k, v)));
        }
    }
}
