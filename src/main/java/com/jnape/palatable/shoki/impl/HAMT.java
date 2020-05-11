package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.adt.product.Product2;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;

import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Flatten.flatten;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Eq.eq;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Find.find;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.impl.Bitmap32.bitIsSet;
import static com.jnape.palatable.shoki.impl.Bitmap32.lowerBits;
import static com.jnape.palatable.shoki.impl.Bitmap32.setBit;
import static com.jnape.palatable.shoki.impl.Bitmap32.unsetBit;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static java.lang.Integer.bitCount;
import static java.util.Arrays.asList;
import static java.util.Collections.singleton;

interface HAMT<K, V> extends Iterable<Tuple2<K, V>> {

    int LEVEL_SIZE = 5;

    HAMT<K, V> put(K key, V value, int keyHash, EquivalenceRelation<? super K> keyEqRel,
                   HashingAlgorithm<? super K> keyHashAlg, int shift);

    V get(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift);

    HAMT<K, V> remove(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift);

    final class Node<K, V> implements HAMT<K, V> {

        private static final Node<?, ?> ROOT = new Node<>(0, new Object[0]);

        private final int      bitmap;
        private final Object[] table;

        public Node(int bitmap, Object[] table) {
            this.bitmap = bitmap;
            this.table  = table;
        }

        @Override
        public V get(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift) {
            int bitmapIndex = bitmapIndex(keyHash, shift);
            return bitIsSet(bitmap, bitmapIndex)
                   ? valueAtIndex(tableIndex(bitmapIndex)).get(key, keyHash, keyEqRel, shift + LEVEL_SIZE)
                   : null;
        }

        @Override
        public Node<K, V> put(K key, V value, int keyHash, EquivalenceRelation<? super K> keyEqRel,
                              HashingAlgorithm<? super K> keyHashAlg, int shift) {
            int bitmapIndex = bitmapIndex(keyHash, shift);
            int tableIndex  = tableIndex(bitmapIndex);
            return bitIsSet(bitmap, bitmapIndex)
                   ? overrideAt(tableIndex,
                                valueAtIndex(tableIndex).put(key, value, keyHash, keyEqRel, keyHashAlg, shift + LEVEL_SIZE))
                   : insertAt(tableIndex, bitmapIndex, new Entry<>(key, value));
        }

        @Override
        public Iterator<Tuple2<K, V>> iterator() {
            @SuppressWarnings("unchecked")
            List<HAMT<K, V>> bodies = (List<HAMT<K, V>>) (Object) asList(table);
            return flatten(bodies).iterator();
        }

        @Override
        public HAMT<K, V> remove(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift) {
            int bitmapIndex = bitmapIndex(keyHash, shift);
            if (!bitIsSet(bitmap, bitmapIndex))
                return this;

            int        tableIndex = tableIndex(bitmapIndex);
            HAMT<K, V> override   = valueAtIndex(tableIndex).remove(key, keyHash, keyEqRel, shift + LEVEL_SIZE);
            return override == null ? deleteAt(bitmapIndex, tableIndex) : overrideAt(tableIndex, override);
        }

        @Override
        public boolean equals(Object other) {
            if (other instanceof Node<?, ?>) {
                Node<?, ?> node = (Node<?, ?>) other;
                return bitmap == node.bitmap &&
                        java.util.Arrays.equals(table, node.table);
            }
            return false;
        }

        private int tableIndex(int bitmapIndex) {
            return bitCount(lowerBits(bitmap, bitmapIndex));
        }

        @SuppressWarnings("unchecked")
        private HAMT<K, V> valueAtIndex(int tableIndex) {
            return (HAMT<K, V>) table[tableIndex];
        }

        private Node<K, V> insertAt(int tableIndex, int bitmapIndex, HAMT<K, V> valueForSlot) {
            return new Node<>(setBit(bitmap, bitmapIndex), Arrays.insertAt(tableIndex, table, valueForSlot));
        }

        private Node<K, V> overrideAt(int tableIndex, HAMT<K, V> valueForSlot) {
            return new Node<>(bitmap, Arrays.overrideAt(tableIndex, table, valueForSlot));
        }

        private Node<K, V> deleteAt(int bitmapIndex, int tableIndex) {
            return new Node<>(unsetBit(bitmap, bitmapIndex), Arrays.deleteAt(tableIndex, table));
        }

        @SuppressWarnings("unchecked")
        static <K, V> HAMT<K, V> rootNode() {
            return (HAMT<K, V>) ROOT;
        }

        private static int bitmapIndex(int keyHash, int shift) {
            return (keyHash >>> shift) & 31;
        }
    }

    final class Entry<K, V> implements HAMT<K, V>, Product2<K, V> {

        private final K k;
        private final V v;

        Entry(K k, V v) {
            this.k = k;
            this.v = v;
        }

        @Override
        public K _1() {
            return k;
        }

        @Override
        public V _2() {
            return v;
        }

        @Override
        public HAMT<K, V> put(K newKey, V newValue, int keyHash,
                              EquivalenceRelation<? super K> keyEqRel, HashingAlgorithm<? super K> keyHashAlg,
                              int shift) {
            if (keyEqRel.apply(newKey, k))
                return new Entry<>(newKey, newValue);

            if (shift > 30)
                return new Collision<>(keyHash, strictStack(this, new Entry<>(newKey, newValue)));

            Integer existingKeyHash = keyHashAlg.apply(k);
            return Node.<K, V>rootNode()
                    .put(k, v, existingKeyHash, keyEqRel, keyHashAlg, shift)
                    .put(newKey, newValue, keyHash, keyEqRel, keyHashAlg, shift);
        }

        @Override
        public Iterator<Tuple2<K, V>> iterator() {
            return singleton(tuple(k, v)).iterator();
        }

        @Override
        public V get(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift) {
            return keyEqRel.apply(key, k) ? v : null;
        }

        @Override
        public HAMT<K, V> remove(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift) {
            return !keyEqRel.apply(key, k) ? this : null;
        }

        @Override
        public boolean equals(Object other) {
            if (other instanceof Entry<?, ?>) {
                Entry<?, ?> entry = (Entry<?, ?>) other;
                return Objects.equals(k, entry.k) && Objects.equals(v, entry.v);
            }
            return false;
        }
    }

    final class Collision<K, V> implements HAMT<K, V> {
        private final int                      keyHash;
        private final StrictStack<Entry<K, V>> kvPairs;

        Collision(int keyHash,
                  StrictStack<Entry<K, V>> kvPairs) {
            this.keyHash = keyHash;
            this.kvPairs = kvPairs;
        }

        @Override
        public HAMT<K, V> put(K key, V value, int keyHash, EquivalenceRelation<? super K> keyEqRel,
                              HashingAlgorithm<? super K> keyHashAlg, int shift) {
            return new Collision<>(keyHash, foldLeft(((s, kv) -> !keyEqRel.apply(key, kv._1()) ? s.cons(kv) : s),
                                                     strictStack(new Entry<>(key, value)),
                                                     kvPairs));
        }

        @Override
        public Iterator<Tuple2<K, V>> iterator() {
            return map(Tuple2::fromEntry, kvPairs).iterator();
        }

        @Override
        public V get(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift) {
            return keyHash == this.keyHash
                   ? find(kvPair -> keyEqRel.apply(key, kvPair._1()), kvPairs).fmap(Entry::_2).orElse(null)
                   : null;
        }

        @Override
        public HAMT<K, V> remove(K key, int keyHash, EquivalenceRelation<? super K> keyEqRel, int shift) {
            if (keyHash != this.keyHash)
                return this;

            StrictStack<Entry<K, V>> withoutKey = foldLeft(((s, kv) -> !keyEqRel.apply(key, kv._1()) ? s.cons(kv) : s),
                                                           strictStack(),
                                                           kvPairs);
            return eq(withoutKey.sizeInfo().getSize(), one())
                   ? withoutKey.iterator().next()
                   : new Collision<>(keyHash, withoutKey);
        }

        @Override
        public boolean equals(Object other) {
            if (other instanceof Collision<?, ?>) {
                Collision<?, ?> collision = (Collision<?, ?>) other;
                return Objects.equals(keyHash, collision.keyHash) &&
                        Objects.equals(kvPairs, collision.kvPairs);
            }
            return false;
        }
    }
}
