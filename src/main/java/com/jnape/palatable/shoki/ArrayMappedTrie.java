package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.internal.Bitmap32;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.shoki.internal.Arrays.*;
import static com.jnape.palatable.shoki.internal.Bitmap32.bitmap32;
import static com.jnape.palatable.shoki.internal.Indices.bitmapIndex;
import static com.jnape.palatable.shoki.internal.Indices.tableIndex;

public final class ArrayMappedTrie<E> implements RandomAccess<Integer, E> {

    private static final ArrayMappedTrie<?> EMPTY = new ArrayMappedTrie<>(Bitmap32.empty(), new Object[0]);

    private final Bitmap32 bitmap;
    private final Object[] array;

    private ArrayMappedTrie(Bitmap32 bitmap, Object[] array) {
        this.bitmap = bitmap;
        this.array = array;
    }

    @Override
    public Maybe<E> get(Integer index) {
        return getForLevel(index, 1);
    }

    public boolean contains(Integer index) {
        return get(index).match(constantly(true), constantly(false));
    }

    public ArrayMappedTrie<E> set(Integer index, E element) {
        return setForLevel(index, element, 1);
    }

    public ArrayMappedTrie<E> remove(Integer index) {
        return removeForLevel(index, 1);
    }

    public boolean isEmpty() {
        return bitmap.bits() == 0;
    }

    private Maybe<E> getForLevel(Integer index, int level) {
        int bitmapIndex = bitmapIndex(bitmap32(index), level);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return Maybe.nothing();

        Object valueAtIndex = array[tableIndex(bitmap, bitmapIndex)];
        if (valueAtIndex instanceof Entry<?>) {
            @SuppressWarnings("unchecked")
            Entry<E> entry = (Entry<E>) valueAtIndex;
            return entry.k == index ? just(entry.v) : Maybe.nothing();
        } else {
            @SuppressWarnings("unchecked")
            ArrayMappedTrie<E> subTrie = (ArrayMappedTrie<E>) valueAtIndex;
            return subTrie.getForLevel(index, level + 1);
        }
    }

    private ArrayMappedTrie<E> setForLevel(Integer index, E element, int level) {
        int bitmapIndex = bitmapIndex(bitmap32(index), level);
        int tableIndex  = tableIndex(bitmap, bitmapIndex);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return new ArrayMappedTrie<>(bitmap.populateAtIndex(bitmapIndex),
                                         insertAt(tableIndex, array, new Entry<>(index, element)));

        Object valueAtIndex = array[tableIndex];
        if (valueAtIndex instanceof Entry<?>) {
            @SuppressWarnings("unchecked")
            Entry<E> entry = (Entry<E>) valueAtIndex;
            return (entry.k == index)
                ? new ArrayMappedTrie<>(bitmap, overrideAt(tableIndex, array, element))
                : new ArrayMappedTrie<>(bitmap,
                                        overrideAt(tableIndex, array, ArrayMappedTrie.<E>empty()
                                            .setForLevel(entry.k, entry.v, level + 1)
                                            .setForLevel(index, element, level + 1)));
        } else {
            @SuppressWarnings("unchecked")
            ArrayMappedTrie<E> subTrie = (ArrayMappedTrie<E>) valueAtIndex;
            return new ArrayMappedTrie<>(bitmap,
                                         overrideAt(tableIndex, array, subTrie.setForLevel(index, element, level + 1)));
        }
    }

    private ArrayMappedTrie<E> removeForLevel(Integer index, int level) {
        int bitmapIndex = bitmapIndex(bitmap32(index), level);
        if (!bitmap.populatedAtIndex(bitmapIndex))
            return this;

        int    tableIndex   = tableIndex(bitmap, bitmapIndex);
        Object valueAtIndex = array[tableIndex];

        if (valueAtIndex instanceof Entry<?>) {
            @SuppressWarnings("unchecked")
            Entry<E> entry = (Entry<E>) valueAtIndex;
            return (entry.k == index)
                ? new ArrayMappedTrie<>(bitmap.evictAtIndex(bitmapIndex), deleteAt(tableIndex, array))
                : this;
        } else {
            @SuppressWarnings("unchecked")
            ArrayMappedTrie<E> subTrie = (ArrayMappedTrie<E>) valueAtIndex;
            ArrayMappedTrie<E> withoutIndex = subTrie.removeForLevel(index, level + 1);
            return withoutIndex.isEmpty()
                ? new ArrayMappedTrie<>(bitmap.evictAtIndex(bitmapIndex), deleteAt(tableIndex, array))
                : new ArrayMappedTrie<>(bitmap, overrideAt(tableIndex, array, withoutIndex));
        }
    }

    @SuppressWarnings("unchecked")
    public static <E> ArrayMappedTrie<E> empty() {
        return (ArrayMappedTrie<E>) EMPTY;
    }

    private static final class Entry<V> {

        private final int k;
        private final V   v;

        private Entry(int k, V v) {
            this.k = k;
            this.v = v;
        }
    }
}
