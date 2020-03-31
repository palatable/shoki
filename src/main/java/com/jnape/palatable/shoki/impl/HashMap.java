package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn1.Empty;
import com.jnape.palatable.lambda.functions.builtin.fn1.Head;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.Map;
import com.jnape.palatable.shoki.api.SizeInfo;

import java.util.Iterator;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Try.trying;
import static com.jnape.palatable.lambda.functions.Fn2.curried;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Size.size;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Cons.cons;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Map.sameEntries;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.Bitmap32.bitmap32;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Arrays.asList;

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
public final class HashMap<K, V> implements Map<Integer, K, V> {

    private static final HashMap<?, ?> EMPTY_OBJECT_DEFAULTS = empty(objectEquals(), objectHashCode());

    private final EquivalenceRelation<K> keyEqRel;
    private final HashingAlgorithm<K>    keyHashAlg;
    private final HAMT<K, V>             hamt;

    private HashMap(EquivalenceRelation<K> keyEqRel, HashingAlgorithm<K> keyHashAlg, HAMT<K, V> hamt) {
        this.keyEqRel = keyEqRel;
        this.keyHashAlg = keyHashAlg;
        this.hamt = hamt;
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
        return hamt.get(key, bitmap32(keyHashAlg.apply(key)), keyEqRel, 1);
    }

    /**
     * {@inheritDoc}
     * If <code>key</code> does not currently associate to a value inside this {@link HashMap}, insert the key/value
     * pair for <code>key</code> and <value>value</value>; otherwise, override the current value associated for
     * <code>key</code> and set it to <code>value</code>. Amortized <code>O(1)</code>.
     *
     * @see HashMap#get(Object)
     * @see HashMap#remove(Object)
     */
    @Override
    public HashMap<K, V> put(K key, V value) {
        return new HashMap<>(keyEqRel, keyHashAlg,
                             hamt.put(key, value, bitmap32(keyHashAlg.apply(key)),
                                      keyEqRel, keyHashAlg, 1));
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
        return new HashMap<>(keyEqRel, keyHashAlg,
                             hamt.remove(key, bitmap32(keyHashAlg.apply(key)), keyEqRel, 1)
                                     .orElse(HAMT.Node.rootNode()));
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
        return foldLeft((keys, kv) -> keys.add(kv._1()), HashSet.empty(), this);
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public StrictStack<V> values() {
        return foldLeft((keys, kv) -> keys.cons(kv._2()), StrictStack.empty(), this);
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
     * <code>O(n)</code>.
     */
    @Override
    public SizeInfo.Known<Integer> sizeInfo() {
        return known(size(this).intValue());
    }

    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return hamt.iterator();
    }

    /**
     * Determine if <code>other</code> is a {@link HashMap} with the
     * {@link Map#sameEntries(Map, Map, EquivalenceRelation) same entries} as this {@link HashMap}, using
     * {@link Object#equals(Object) Object equality} to determine value equivalence. <code>O(n)</code>.
     *
     * @param other the {@link Object} to check for equality
     * @return the equality outcome
     * @see Map#sameEntries(Map, Map, EquivalenceRelation)
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof HashMap<?, ?> &&
                trying(() -> sameEntries(this, downcast(other), objectEquals()))
                        .catching(ClassCastException.class, constantly(false))
                        .orThrow();
    }

    /**
     * Compute the corresponding {@link Object#hashCode() hash code} for this {@link HashMap}. <code>O(n)</code>.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return foldLeft(Integer::sum, 0, map(into((k, v) -> 31 * keyHashAlg.apply(k) + Objects.hashCode(v)), this));
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
     * Create an empty {@link HashMap} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm} for its
     * keys.
     *
     * @param keyEquivalenceRelation the {@link EquivalenceRelation}
     * @param keyHashingAlgorithm    the {@link HashingAlgorithm}
     * @param <K>                    the key type
     * @param <V>                    the value type
     * @return the empty {@link HashMap}
     */
    public static <K, V> HashMap<K, V> empty(EquivalenceRelation<K> keyEquivalenceRelation,
                                             HashingAlgorithm<K> keyHashingAlgorithm) {
        return new HashMap<>(keyEquivalenceRelation, keyHashingAlgorithm, HAMT.Node.rootNode());
    }

    /**
     * The empty singleton {@link HashMap} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively, for its keys.
     *
     * @param <K> the key type
     * @param <V> the value type
     * @return the empty {@link HashMap}
     */
    @SuppressWarnings("unchecked")
    public static <K, V> HashMap<K, V> empty() {
        return (HashMap<K, V>) EMPTY_OBJECT_DEFAULTS;
    }

    /**
     * Create a new {@link HashMap} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm} for its
     * keys, populated by one or more given entries.
     *
     * @param keyEquivalenceRelation the {@link EquivalenceRelation}
     * @param keyHashingAlgorithm    the {@link HashingAlgorithm}
     * @param entry                  the first entry
     * @param entries                the rest of the entries
     * @param <K>                    the key type
     * @param <V>                    the value type
     * @return the populated {@link HashMap}
     */
    @SafeVarargs
    public static <K, V> HashMap<K, V> of(EquivalenceRelation<K> keyEquivalenceRelation,
                                          HashingAlgorithm<K> keyHashingAlgorithm,
                                          Tuple2<K, V> entry, Tuple2<K, V>... entries) {
        return foldLeft(curried(m -> into(m::put)),
                        empty(keyEquivalenceRelation, keyHashingAlgorithm),
                        cons(entry, asList(entries)));
    }

    /**
     * Create a new {@link HashMap} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively, for its keys, populated by one or more given entries.
     *
     * @param entry   the first entry
     * @param entries the rest of the entries
     * @param <K>     the key type
     * @param <V>     the value type
     * @return the populated {@link HashMap}
     */
    @SafeVarargs
    public static <K, V> HashMap<K, V> of(Tuple2<K, V> entry, Tuple2<K, V>... entries) {
        return of(objectEquals(), objectHashCode(), entry, entries);
    }
}
