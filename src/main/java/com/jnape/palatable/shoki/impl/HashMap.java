package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn1.Empty;
import com.jnape.palatable.lambda.functions.builtin.fn1.Head;
import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.Map;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Set;
import com.jnape.palatable.shoki.api.SizeInfo.Known;

import java.util.Iterator;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.maybe;
import static com.jnape.palatable.lambda.adt.Try.trying;
import static com.jnape.palatable.lambda.functions.Fn2.curried;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.hash;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Map.EquivalenceRelations.entries;
import static com.jnape.palatable.shoki.api.Map.HashingAlgorithms.entries;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.HAMT.Node.rootNode;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static java.lang.String.format;
import static java.lang.String.join;

/**
 * A <a href="https://lampwww.epfl.ch/papers/idealhashtrees.pdf" target="_new">hash array mapped trie</a>
 * implementation of a {@link Map}, offering amortized <code>O(1)</code> {@link HashMap#get(Object) search},
 * {@link HashMap#put(Object, Object) insert}, and {@link HashMap#remove(Object) delete} operations with small
 * worst-case lookup constants (assuming good {@link HashingAlgorithm hash distribution}), and optimizations to make
 * the cost of a miss strictly upper-bounded by the cost of a hit (and generally, significantly cheaper).
 * <p>
 * A <a href="https://en.wikipedia.org/wiki/Trie" target="_new">trie</a> (commonly referred to as a "prefix tree", and
 * confusingly pronounced "tree") is a search tree that branches only where common supersets of sibling nodes diverge.
 * As an example, a trie storing the strings "foo", "bar", and "baz" might be visually represented as:
 * <pre>
 *      .
 *     / \
 *    /   \
 * foo     ba
 *        /  \
 *       r    z
 * </pre>
 * An <a href="https://infoscience.epfl.ch/record/64394" target="_new">array mapped trie</a> is a trie that offers a
 * map-like interface from integral values - representing array indices - to the element type that is stored, where the
 * bitmaps of the integrals are used as the common node prefix. As an example, an array mapped trie storing the
 * integral/string pairs <code>(1, "foo"), (2, "bar"), (3, "baz")</code> might be visually represented as:
 * <pre>
 *      .
 *     / \
 *    /   \
 *  01     1
 * foo    / \
 *       /   \
 *      0     1
 *     bar   baz
 * </pre>
 * <p>
 * This trie additionally maintains a memory-efficient bitmap representation of the table space cardinality and
 * populated indices to offer efficient lookup, insertion, and removal operations, so the actual implementation using
 * arrays could be modelled as follows:
 * <pre>
 * Trie={bitmap: 0b11,
 *       // index 0 is populated (value)
 *       // index 1 is populated (sub-trie)
 *       // cardinality of this array is 2 (POPCNT(0b11))
 *       arr: [Value="foo"
 *             Trie={bitmap: 0b11,
 *                   // index 0 is populated (value)
 *                   // index 1 is populated (value)
 *                   // cardinality of this array is 2 (POPCNT(0b11))
 *                   arr: [Value="bar", Value="baz"]
 *                  }
 *            ]
 *      }
 * </pre>
 * <p>
 * This implementation can be further improved by ensuring that all internal arrays are dense, regardless of how the
 * populated indices would otherwise distribute across the table space, resulting in an ideally memory efficient space
 * layout.
 * <p>
 * With all of these definitions in place, a {@link HashMap} is simply an array mapped trie that maps any arbitrary
 * type <code>K</code> to any arbitrary type <code>V</code> by hashing <code>K</code> into an integral value, which is
 * then used to index into the corresponding internal array table space. However, because the integrals are the results
 * of a hash function, full hash collisions between different keys are possible, so in addition to the standard
 * trie/value array slot occupants, there may instead be a collision stack, representing two or more entries who's keys
 * are different but have produced full hash collisions. The presence of collision stacks can materially slow down the
 * typical {@link Map} operations, so a {@link HashingAlgorithm hashing algorithm} offering reasonably good uniformity
 * of distribution from <code>K -&gt; {@link Integer}</code> is important to maintain optimal performance
 * characteristics.
 * <p>
 * Finally, a {@link HashMap} can be configured upon creation with custom {@link EquivalenceRelation equality} and
 * {@link HashingAlgorithm hashing} semantics, obviating the need to rely on
 * {@link Object#equals(Object) Object equality} and {@link Object#hashCode() Object hashCode} unless specifically
 * desired.
 *
 * @param <K> the key type
 * @param <V> the value type
 * @see Map
 * @see EquivalenceRelation
 * @see HashingAlgorithm
 */
public final class HashMap<K, V> implements Map<Natural, K, V> {

    private static final HashMap<?, ?> EMPTY_OBJECT_DEFAULTS =
            new HashMap<>(objectEquals(), objectHashCode(), rootNode());

    private final EquivalenceRelation<? super K> keyEqRel;
    private final HashingAlgorithm<? super K>    keyHashAlg;
    private final HAMT<K, V>                     hamt;

    private volatile Natural size;
    private volatile Integer hashCode;

    private HashMap(EquivalenceRelation<? super K> keyEqRel, HashingAlgorithm<? super K> keyHashAlg, HAMT<K, V> hamt) {
        this.keyEqRel   = keyEqRel;
        this.keyHashAlg = keyHashAlg;
        this.hamt       = hamt;
    }

    /**
     * {@inheritDoc}
     * If <code>key</code> is associated to a value inside this {@link HashMap}, retrieve
     * {@link Maybe#just(Object) just} the value it maps to; otherwise, return {@link Maybe#nothing() nothing}.
     * Amortized <code>O(1)</code>.
     *
     * @see HashMap#put(Object, Object)
     * @see HashMap#remove(Object)
     */
    @Override
    public Maybe<V> get(K key) {
        return maybe(hamt.get(key, keyHashAlg.apply(key), keyEqRel, 0));
    }

    /**
     * {@inheritDoc}
     * If <code>key</code> does not currently associate to a value inside this {@link HashMap}, insert the key/value
     * pair for <code>key</code> and <code>value</code>; otherwise, override the current value associated for
     * <code>key</code> and set it to <code>value</code>. Amortized <code>O(1)</code>.
     *
     * @see HashMap#get(Object)
     * @see HashMap#remove(Object)
     */
    @Override
    public HashMap<K, V> put(K key, V value) {
        return new HashMap<>(keyEqRel, keyHashAlg,
                             hamt.put(key, value, keyHashAlg.apply(key), keyEqRel, keyHashAlg, 0));
    }

    /**
     * {@inheritDoc}
     * If <code>key</code> does not currently associate to a value inside this {@link HashMap}, return this same
     * {@link HashMap}; otherwise, remove the existing association, back-propagating any compression that can be
     * achieved as a result of the removed entry. Amortized <code>O(1)</code>.
     *
     * @see HashMap#get(Object)
     * @see HashMap#put(Object, Object)
     */
    @Override
    public HashMap<K, V> remove(K key) {
        HAMT<K, V> removed = hamt.remove(key, keyHashAlg.apply(key), keyEqRel, 0);
        return new HashMap<>(keyEqRel, keyHashAlg, removed != null ? removed : rootNode());
    }

    /**
     * {@inheritDoc}
     * <code>True</code> if <code>key</code> is associated to a value in this {@link HashMap}; <code>false</code>
     * otherwise. Amortized <code>O(1)</code>.
     */
    @Override
    public boolean contains(K key) {
        return get(key).match(constantly(false), constantly(true));
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public HashSet<K> keys() {
        return foldLeft((keys, kv) -> keys.add(kv._1()), hashSet(keyEqRel, keyHashAlg), this);
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public StrictQueue<V> values() {
        return foldLeft((values, kv) -> values.snoc(kv._2()), strictQueue(), this);
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public Maybe<Tuple2<K, V>> head() {
        return Head.head(this);
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public HashMap<K, V> tail() {
        return head()
                .fmap(into(curried(headKey -> constantly(remove(headKey)))))
                .orElse(this);
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public boolean isEmpty() {
        return Empty.empty(hamt);
    }

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public HashMap<K, V> merge(Map<Natural, K, V> other, Semigroup<V> semigroup) {
        return (HashMap<K, V>) Map.super.merge(other, semigroup);
    }

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public HashMap<K, V> removeAll(Set<Natural, K> keys) {
        return (HashMap<K, V>) Map.super.removeAll(keys);
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    @SuppressWarnings("DuplicatedCode")
    public Known<Natural> sizeInfo() {
        Natural size = this.size;
        if (size == null) {
            synchronized (this) {
                size = this.size;
                if (size == null) {
                    this.size = size = foldLeft((s, __) -> s.inc(), (Natural) zero(), this);
                }
            }
        }
        return known(size);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return hamt.iterator();
    }

    /**
     * Determine if <code>other</code> is a {@link HashMap} with the
     * {@link Map.EquivalenceRelations#entries(EquivalenceRelation) same entries} as this {@link HashMap}, using
     * {@link Object#equals(Object) Object equality} to determine value equivalence. <code>O(n)</code>.
     *
     * @param other the {@link Object} to check for equality
     * @return the equality outcome
     * @see Map.EquivalenceRelations#entries(EquivalenceRelation)
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof HashMap<?, ?> &&
                trying(() -> equivalent(entries(objectEquals()), this, downcast(other)))
                        .catching(ClassCastException.class, constantly(false))
                        .orThrow();
    }

    /**
     * Compute the corresponding {@link Object#hashCode() hash code} for this {@link HashMap}.
     * Amortized <code>O(1)</code>.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        Integer hashCode = this.hashCode;
        if (hashCode == null) {
            synchronized (this) {
                hashCode = this.hashCode;
                if (hashCode == null) {
                    this.hashCode = hashCode = hash(entries(keyHashAlg, objectHashCode()), this);
                }
            }
        }
        return hashCode;
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public String toString() {
        return "HashMap[" + join(", ", map(into((k, v) -> format("(%s=%s)", k, v)), this)) + "]";
    }

    /**
     * Create a {@link HashMap} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm} for its
     * keys, populated by zero or more given entries.
     *
     * @param keyEquivalenceRelation the {@link EquivalenceRelation}
     * @param keyHashingAlgorithm    the {@link HashingAlgorithm}
     * @param entries                the entries
     * @param <K>                    the key type
     * @param <V>                    the value type
     * @return the {@link HashMap}
     */
    @SafeVarargs
    public static <K, V> HashMap<K, V> hashMap(EquivalenceRelation<? super K> keyEquivalenceRelation,
                                               HashingAlgorithm<? super K> keyHashingAlgorithm,
                                               Tuple2<K, V>... entries) {
        return hashMap(new HashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, rootNode()), entries);
    }

    /**
     * Create a {@link HashMap} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively, for its keys, populated by zero or more given entries.
     *
     * @param entries the entries
     * @param <K>     the key type
     * @param <V>     the value type
     * @return the {@link HashMap}
     */
    @SafeVarargs
    public static <K, V> HashMap<K, V> hashMap(Tuple2<K, V>... entries) {
        @SuppressWarnings("unchecked") HashMap<K, V> empty = (HashMap<K, V>) EMPTY_OBJECT_DEFAULTS;
        return hashMap(empty, entries);
    }

    @SafeVarargs
    private static <K, V> HashMap<K, V> hashMap(HashMap<K, V> hashMap, Tuple2<K, V>... entries) {
        for (Tuple2<K, V> entry : entries)
            hashMap = entry.into(hashMap::put);
        return hashMap;
    }
}
