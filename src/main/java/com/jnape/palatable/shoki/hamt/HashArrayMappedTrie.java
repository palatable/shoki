package com.jnape.palatable.shoki.hamt;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn1.Empty;
import com.jnape.palatable.lambda.functions.builtin.fn1.Head;
import com.jnape.palatable.shoki.ImmutableHashSet;
import com.jnape.palatable.shoki.ImmutableStack;
import com.jnape.palatable.shoki.SizeInfo;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.Map;

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
import static com.jnape.palatable.shoki.SizeInfo.known;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Map.sameEntries;
import static com.jnape.palatable.shoki.hamt.Bitmap32.bitmap32;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Arrays.asList;

/**
 * A <a href="https://lampwww.epfl.ch/papers/idealhashtrees.pdf" target="_new">hash array mapped trie</a>
 * implementation of a {@link Map}, offering amortized <code>O(1)</code> {@link HashArrayMappedTrie#get(Object) search},
 * {@link HashArrayMappedTrie#put(Object, Object) insert}, and {@link HashArrayMappedTrie#remove(Object) delete}
 * operations with small worst-case lookup constants (assuming good {@link HashingAlgorithm hash distribution}), and
 * optimizations to make the cost of a miss strictly upper-bounded by the cost of a hit (and generally, significantly
 * cheaper).
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
 *       // cardinality of this array is 2 (ctpop(0b11))
 *       arr: [Value="foo"
 *             Trie={bitmap: 0b11,
 *                   // index 0 is populated (value)
 *                   // index 1 is populated (value)
 *                   // cardinality of this array is 2 (ctpop(0b11))
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
 * With all of these definitions in place, a {@link HashArrayMappedTrie} is simply an array mapped trie that maps any
 * arbitrary type <code>K</code> to any arbitrary type <code>V</code> by hashing <code>K</code> into an integral value,
 * which is then used to index into the corresponding internal array table space. However, because the integrals are
 * the results of a hash function, full hash collisions between different keys are possible, so in addition to the
 * standard trie/value array slot occupants, there may instead be a collision stack, representing two or more entries
 * who's keys are different but have produced full hash collisions. The presence of collision stacks can materially
 * slow down the typical {@link Map} operations, so a {@link HashingAlgorithm hashing algorithm} offering reasonably
 * good uniformity of distribution from <code>K -&gt; {@link Integer}</code> is important to maintain optimal
 * performance characteristics.
 * <p>
 * Finally, a {@link HashArrayMappedTrie} can be configured upon creation with custom
 * {@link EquivalenceRelation equality} and {@link HashingAlgorithm hashing} semantics, avoiding the need to rely on
 * {@link Object#equals(Object) Object equality} and {@link Object#hashCode() Object hashCode} unless specifically
 * desired.
 *
 * @param <K> the key type
 * @param <V> the value type
 * @see Map
 * @see EquivalenceRelation
 * @see HashingAlgorithm
 */
public final class HashArrayMappedTrie<K, V> implements Map<Integer, K, V> {

    private static final HashArrayMappedTrie<?, ?> EMPTY_OBJECT_DEFAULTS = empty(objectEquals(), objectHashCode());

    private final EquivalenceRelation<K> keyEqRel;
    private final HashingAlgorithm<K>    keyHashAlg;
    private final Body<K, V>             body;

    private HashArrayMappedTrie(EquivalenceRelation<K> keyEqRel, HashingAlgorithm<K> keyHashAlg, Body<K, V> body) {
        this.keyEqRel = keyEqRel;
        this.keyHashAlg = keyHashAlg;
        this.body = body;
    }

    /**
     * {@inheritDoc}
     * If <code>key</code> is associated to a value inside this {@link HashArrayMappedTrie}, retrieve
     * {@link Maybe#just(Object) just} the value it maps to; otherwise, return {@link Maybe#nothing() nothing}.
     * Amortized <code>O(1)</code>.
     *
     * @see HashArrayMappedTrie#put(Object, Object)
     * @see HashArrayMappedTrie#remove(Object)
     */
    @Override
    public Maybe<V> get(K key) {
        return body.get(key, bitmap32(keyHashAlg.apply(key)), keyEqRel, 1);
    }

    /**
     * {@inheritDoc}
     * If <code>key</code> does not currently associate to a value inside this {@link HashArrayMappedTrie}, insert the
     * key/value pair for <code>key</code> and <value>value</value>; otherwise, override the current value associated
     * for <code>key</code> and set it to <code>value</code>. Amortized <code>O(1)</code>.
     *
     * @see HashArrayMappedTrie#get(Object)
     * @see HashArrayMappedTrie#remove(Object)
     */
    @Override
    public HashArrayMappedTrie<K, V> put(K key, V value) {
        return new HashArrayMappedTrie<>(keyEqRel, keyHashAlg,
                                         body.put(key, value, bitmap32(keyHashAlg.apply(key)),
                                                  keyEqRel, keyHashAlg, 1));
    }

    /**
     * {@inheritDoc}
     * If <code>key</code> does not currently associate to a value inside this {@link HashArrayMappedTrie}, return this
     * same {@link HashArrayMappedTrie}; otherwise, remove the existing association, back-propagating any compression
     * that can be achieved as a result of the removed entry. Amortized <code>O(1)</code>.
     *
     * @see HashArrayMappedTrie#get(Object)
     * @see HashArrayMappedTrie#put(Object, Object)
     */
    @Override
    public HashArrayMappedTrie<K, V> remove(K key) {
        return new HashArrayMappedTrie<>(keyEqRel, keyHashAlg,
                                         body.remove(key, bitmap32(keyHashAlg.apply(key)), keyEqRel, 1)
                                                 .orElse(Body.Node.rootNode()));
    }

    /**
     * {@inheritDoc}
     * <code>True</code> if <code>key</code> is associated to a value in this {@link HashArrayMappedTrie};
     * <code>false</code> otherwise. Amortized <code>O(1)</code>.
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
    public ImmutableHashSet<K> keys() {
        return foldLeft((keys, kv) -> keys.add(kv._1()), ImmutableHashSet.empty(), this);
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public ImmutableStack<V> values() {
        return foldLeft((keys, kv) -> keys.cons(kv._2()), ImmutableStack.empty(), this);
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
    public HashArrayMappedTrie<K, V> tail() {
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
        return Empty.empty(body);
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
    public String toString() {
        return "HashArrayMappedTrie["
                + join(", ", map(into((k, v) -> format("(%s=%s)", k, v)), this))
                + "]";
    }

    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return body.iterator();
    }

    /**
     * Determine if <code>other</code> is a {@link HashArrayMappedTrie} with the
     * {@link Map#sameEntries(Map, Map, EquivalenceRelation) same entries} as this {@link HashArrayMappedTrie}, using
     * {@link Object#equals(Object) Object equality} to determine value equivalence. <code>O(n)</code>.
     *
     * @see Map#sameEntries(Map, Map, EquivalenceRelation)
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof HashArrayMappedTrie<?, ?> &&
                trying(() -> sameEntries(this, downcast(other), objectEquals()))
                        .catching(ClassCastException.class, constantly(false))
                        .orThrow();
    }

    /**
     * Compute the corresponding {@link Object#hashCode() hash code} for this {@link HashArrayMappedTrie}.
     * <code>O(n)</code>.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return foldLeft(Integer::sum, 0, map(into((k, v) -> 31 * keyHashAlg.apply(k) + Objects.hashCode(v)), this));
    }

    /**
     * Create an empty {@link HashArrayMappedTrie} using the given {@link EquivalenceRelation} and
     * {@link HashingAlgorithm} for its keys.
     *
     * @param keyEquivalenceRelation the {@link EquivalenceRelation}
     * @param keyHashingAlgorithm    the {@link HashingAlgorithm}
     * @param <K>                    the key type
     * @param <V>                    the value type
     * @return the empty {@link HashArrayMappedTrie}
     */
    public static <K, V> HashArrayMappedTrie<K, V> empty(EquivalenceRelation<K> keyEquivalenceRelation,
                                                         HashingAlgorithm<K> keyHashingAlgorithm) {
        return new HashArrayMappedTrie<>(keyEquivalenceRelation, keyHashingAlgorithm, Body.Node.rootNode());
    }

    /**
     * The empty singleton {@link HashArrayMappedTrie} using {@link Objects#equals(Object, Object) Object equality}
     * and {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and
     * {@link HashingAlgorithm}, respectively, for its keys.
     *
     * @param <K> the key type
     * @param <V> the value type
     * @return the empty {@link HashArrayMappedTrie}
     */
    @SuppressWarnings("unchecked")
    public static <K, V> HashArrayMappedTrie<K, V> empty() {
        return (HashArrayMappedTrie<K, V>) EMPTY_OBJECT_DEFAULTS;
    }

    /**
     * Create a new {@link HashArrayMappedTrie} using the given {@link EquivalenceRelation} and
     * {@link HashingAlgorithm} for its keys and populated by one or more given entries.
     *
     * @param keyEquivalenceRelation the {@link EquivalenceRelation}
     * @param keyHashingAlgorithm    the {@link HashingAlgorithm}
     * @param entry                  the first entry
     * @param entries                the rest of the entries
     * @param <K>                    the key type
     * @param <V>                    the value type
     * @return the populated {@link HashArrayMappedTrie}
     */
    @SafeVarargs
    public static <K, V> HashArrayMappedTrie<K, V> hashArrayMappedTrie(EquivalenceRelation<K> keyEquivalenceRelation,
                                                                       HashingAlgorithm<K> keyHashingAlgorithm,
                                                                       Tuple2<K, V> entry, Tuple2<K, V>... entries) {
        return foldLeft(curried(m -> into(m::put)),
                        empty(keyEquivalenceRelation, keyHashingAlgorithm),
                        cons(entry, asList(entries)));
    }

    /**
     * Create a new {@link HashArrayMappedTrie} using {@link Objects#equals(Object, Object) Object equality}
     * and {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and
     * {@link HashingAlgorithm}, respectively, for its keys, and populated by one or more given entries.
     *
     * @param entry   the first entry
     * @param entries the rest of the entries
     * @param <K>     the key type
     * @param <V>     the value type
     * @return the populated {@link HashArrayMappedTrie}
     */
    @SafeVarargs
    public static <K, V> HashArrayMappedTrie<K, V> hashArrayMappedTrie(Tuple2<K, V> entry, Tuple2<K, V>... entries) {
        return hashArrayMappedTrie(objectEquals(), objectHashCode(), entry, entries);
    }
}
