package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static java.lang.System.arraycopy;

public final class HAMT<K, V> {

    private final Bitmap32 bitmap;
    private final Object[] table;

    HAMT(Bitmap32 bitmap, Object[] table) {
        this.bitmap = bitmap;
        this.table = table;
    }

    public Maybe<V> get(K k) {
        return getForHashLevel(k, Bitmap32.hash(k), 1);
    }

    public HAMT<K, V> put(K k, V v) {
        return putForHashLevel(new Entry<>(Bitmap32.hash(k), k, v), 1);
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
        }

        throw new UnsupportedOperationException("collisions not yet suppported");
    }

    private HAMT<K, V> putForHashLevel(Entry<K, V> entry, int level) {
        int index = entry.keyHash.index(level);
        if (!bitmap.populatedAtIndex(index))
            return fastInsert(entry, index);

        return replaceOrPropagate(entry, index, level);
    }

    private HAMT<K, V> replaceOrPropagate(Entry<K, V> newEntry, int index, int level) {
        Object obj = table[index];
        if (obj instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> existingEntry = (Entry<K, V>) obj;
            return Objects.equals(existingEntry.k, newEntry.k)
                   ? fastInsert(newEntry, index)
                   : propagateBoth(existingEntry, newEntry, index, level);
        }

        throw new UnsupportedOperationException("Sub-tries not yet supported");
    }

    private HAMT<K, V> propagateBoth(Entry<K, V> existingEntry, Entry<K, V> newEntry, int index, int level) {
        int nextLevel = level + 1;
        HAMT<K, V> subTrie = HAMT.<K, V>empty()
                .putForHashLevel(existingEntry, nextLevel)
                .putForHashLevel(newEntry, nextLevel);

        Object[] copy = new Object[32];
        if (index == 0) {
            arraycopy(table, 1, copy, 1, 31);
            copy[0] = subTrie;
        } else if (index == 31) {
            arraycopy(table, 0, copy, 0, 31);
            copy[31] = subTrie;
        } else {
            arraycopy(table, 0, copy, 0, index);
            copy[index] = subTrie;
            arraycopy(table, index + 1, copy, index + 1, 31 - index);
        }

        return new HAMT<>(bitmap.populateAtIndex(index), copy);
    }

    private HAMT<K, V> fastInsert(Entry<K, V> entry, int index) {
        Object[] copy = new Object[32];
        if (index == 0) {
            arraycopy(table, 1, copy, 1, 31);
            copy[0] = entry;
        } else if (index == 31) {
            arraycopy(table, 0, copy, 0, 31);
            copy[31] = entry;
        } else {
            arraycopy(table, 0, copy, 0, index);
            copy[index] = entry;
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
}
