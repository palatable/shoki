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
        return getForHashLevel(k, 1, Bitmap32.hash(k));
    }

    public HAMT<K, V> put(K k, V v) {
        return putForHashLevel(k, v, 1, Bitmap32.hash(k));
    }

    private Maybe<V> getForHashLevel(K k, int level, Bitmap32 keyHash) {
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
            return subTrie.getForHashLevel(k, level + 1, keyHash);
        }

        throw new UnsupportedOperationException("collisions not yet suppported");
    }

    private HAMT<K, V> putForHashLevel(K k, V v, int level, Bitmap32 keyHash) {
        int index = keyHash.index(level);
        if (!bitmap.populatedAtIndex(index))
            return fastInsert(k, v, index);

        return replaceOrPropagate(k, v, index, keyHash, level);
    }

    private HAMT<K, V> replaceOrPropagate(K k, V v, int index, Bitmap32 keyHash, int level) {
        Object obj = table[index];
        if (obj instanceof Entry<?, ?>) {
            @SuppressWarnings("unchecked")
            Entry<K, V> entry = (Entry<K, V>) obj;
            return Objects.equals(entry.k, k)
                   ? fastInsert(k, v, index)
                   : propagate(entry, k, v, index, keyHash, level);
        }

        throw new UnsupportedOperationException("Sub-tries not yet supported");
    }

    private HAMT<K, V> propagate(Entry<K, V> entry, K k, V v, int index, Bitmap32 newKeyHash, int level) {
        int nextLevel = level + 1;
        HAMT<K, V> subTrie = HAMT.<K, V>empty()
                .putForHashLevel(entry.k, entry.v, nextLevel, Bitmap32.hash(entry.k))
                .putForHashLevel(k, v, nextLevel, newKeyHash);

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

    private HAMT<K, V> fastInsert(K k, V v, int index) {
        Object[] copy = new Object[32];
        if (index == 0) {
            arraycopy(table, 1, copy, 1, 31);
            copy[0] = new Entry<>(k, v);
        } else if (index == 31) {
            arraycopy(table, 0, copy, 0, 31);
            copy[31] = new Entry<>(k, v);
        } else {
            arraycopy(table, 0, copy, 0, index);
            copy[index] = new Entry<>(k, v);
            arraycopy(table, index + 1, copy, index + 1, 31 - index);
        }
        return new HAMT<>(bitmap.populateAtIndex(index), copy);
    }

    public String bitmap() {
        return bitmap.toString();
    }

    public static <K, V> HAMT<K, V> empty() {
        return new HAMT<>(Bitmap32.empty(), new Object[32]);
    }

    private static final class Entry<K, V> {

        private final K k;
        private final V v;

        private Entry(K k, V v) {
            this.k = k;
            this.v = v;
        }
    }
}
