package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn1.Downcast;
import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.Map;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Set;
import com.jnape.palatable.shoki.api.SizeInfo;
import com.jnape.palatable.shoki.api.SortedCollection;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.maybe;
import static com.jnape.palatable.lambda.adt.Try.trying;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
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
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static com.jnape.palatable.shoki.impl.TreeSet.treeSet;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Comparator.naturalOrder;

/**
 * A <a href="https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf">red-black tree</a>
 * implementation of a {@link Map} that is also a {@link SortedCollection} over its entries, offering worst-case
 * <code>O(log2n)</code> {@link TreeMap#get(Object) search}, {@link TreeMap#put(Object, Object) insert},
 * {@link TreeMap#remove(Object) delete}, {@link TreeMap#min() min entry} and {@link TreeMap#max() max entry}
 * operations.
 * <p>
 * A <a href="https://en.wikipedia.org/wiki/Red%E2%80%93black_tree">red-black tree</a> is an implementation of a
 * self-balancing <a href="https://en.wikipedia.org/wiki/Binary_search_tree">binary search tree</a> that relies on node
 * coloring constraints to maintain an approximately balanced spine. These constraints can be concisely stated via the
 * following two properties:
 * <ul>
 * <li>The same number of black nodes must be traversed to reach any given leaf in the tree</li>
 * <li>No red nodes may be a direct child of another red node</li>
 * </ul>
 * A red-black tree storing the range of natural numbers <code>[1..5]</code> might be visually represented as:
 * <pre>
 *     2
 *    (B)
 *    / \
 *   /   \
 *  1     4
 * (B)   (R)
 *       / \
 *      3   5
 *     (B) (B)
 * </pre>
 * Because of a red-black tree's flexibility in allowing approximately balanced spines, re-balancing can be done less
 * frequently than a perfectly balanced binary search tree, allowing less work on average to be performed during
 * updates, while still maintaining approximately equivalent search characteristics (even in a
 * <a href="http://matt.might.net/articles/red-black-delete/">purely-functional setting</a>).
 * <p>
 * Additionally, this {@link TreeMap} can be configured with a custom {@link Comparator} so as to not require its
 * elements to necessarily support {@link Comparable comparability} <em>a priori</em>. However, the consequence of the
 * primacy of the ordering constraint is that there can be no distinction between "equivalent" and "comparably
 * equivalent" in the context of a {@link TreeMap}, so any key collision between two "different" keys that are
 * considered to be comparably equal always results in an override.
 *
 * @param <K> the key type
 * @param <V> the value type
 */
public final class TreeMap<K, V> implements Map<Natural, K, V>, SortedCollection<Natural, Tuple2<K, V>, K> {
    private final Comparator<? super K> keyComparator;
    private final RedBlackTree<K, V>    tree;

    private volatile Natural size;
    private volatile Integer hashCode;

    private TreeMap(Comparator<? super K> keyComparator, RedBlackTree<K, V> tree) {
        this.keyComparator = keyComparator;
        this.tree          = tree;
    }

    /**
     * The {@link Comparator} used for the keys stored in this {@link TreeMap}.
     *
     * @return the key {@link Comparator}
     */
    public Comparator<? super K> keyComparator() {
        return keyComparator;
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public boolean isEmpty() {
        return tree.isEmpty();
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<V> get(K k) {
        return maybe(tree.get(k, keyComparator));
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<Tuple2<K, V>> min() {
        return maybe(tree.min());
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<Tuple2<K, V>> max() {
        return maybe(tree.max());
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public boolean contains(K k) {
        return get(k).match(constantly(false), constantly(true));
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public TreeMap<K, V> put(K k, V v) {
        return new TreeMap<>(keyComparator, tree.insert(k, v, keyComparator));
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public TreeMap<K, V> remove(K k) {
        return new TreeMap<>(keyComparator, tree.delete(k, keyComparator));
    }

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public TreeMap<K, V> merge(Map<Natural, K, V> other, Semigroup<V> semigroup) {
        return (TreeMap<K, V>) Map.super.merge(other, semigroup);
    }

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public TreeMap<K, V> removeAll(Set<Natural, K> keys) {
        return (TreeMap<K, V>) Map.super.removeAll(keys);
    }

    /**
     * {@inheritDoc}
     * The resulting {@link TreeSet} employs the same {@link Comparator comparison relation} as the backing
     * {@link TreeMap}.
     */
    @Override
    public TreeSet<K> keys() {
        return foldLeft((keys, kv) -> keys.add(kv._1()), treeSet(keyComparator), this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public StrictQueue<V> values() {
        return foldLeft((values, kv) -> values.snoc(kv._2()), strictQueue(), this);
    }

    /**
     * Sort the keys in this {@link TreeMap} according to the given <code>comparator</code>. <code>O(nlogn)</code>.
     *
     * @param comparator the {@link Comparator}
     * @return the updated {@link TreeMap}
     */
    @Override
    public TreeMap<K, V> sort(Comparator<? super K> comparator) {
        return Objects.equals(keyComparator, comparator)
               ? this
               : foldLeft((m, kv) -> kv.into(m::put), treeMap(comparator), this);
    }

    /**
     * Invert the {@link Comparator ordering relation} used in this {@link TreeMap}. <code>O(n)</code>.
     *
     * @return the reversed {@link TreeMap}
     */
    @Override
    public TreeMap<K, V> reverse() {
        return new TreeMap<>(keyComparator.reversed(), tree.reverse());
    }

    /**
     * {@inheritDoc}
     * In the context of a {@link TreeMap}, this is the {@link TreeMap#min() min element}.
     * <code>O(log2n)</code>.
     *
     * @see TreeMap#min()
     */
    @Override
    public Maybe<Tuple2<K, V>> head() {
        return min();
    }

    /**
     * {@inheritDoc}
     * In the context of a {@link TreeMap}, this is all of the elements of the {@link TreeMap} without the
     * {@link TreeMap#min() min element}.
     * <code>O(log2n)</code>.
     */
    @Override
    public TreeMap<K, V> tail() {
        return new TreeMap<>(keyComparator, tree.deleteMin());
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public SizeInfo.Known<Natural> sizeInfo() {
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
     * Produce an <a href="https://en.wikipedia.org/wiki/Tree_traversal#In-order_(LNR)">in-order</a> {@link Iterator}
     * of the entries in this {@link TreeMap} according to its ordering relation.
     *
     * @return the in-order {@link Iterator}
     */
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return tree.iterator();
    }

    /**
     * Determine if <code>other</code> is a {@link TreeMap} with the same comparison relation and
     * {@link Map.EquivalenceRelations#entries(EquivalenceRelation) same entries} as this {@link TreeMap}, using
     * {@link Object#equals(Object) Object equality} to determine value equivalence. <code>O(nlogn)</code>.
     *
     * @param other the {@link Object} to check for equality
     * @return the equality outcome
     * @see Map.EquivalenceRelations#entries(EquivalenceRelation)
     */
    @Override
    public boolean equals(Object other) {
        if (other instanceof TreeMap<?, ?>) {
            TreeMap<?, ?> that = (TreeMap<?, ?>) other;
            return equivalent(objectEquals(), keyComparator, that.keyComparator)
                    && trying(() -> equivalent(entries(objectEquals()), this,
                                               Downcast.<TreeMap<K, V>, TreeMap<?, ?>>downcast(that)))
                    .catching(ClassCastException.class, constantly(false))
                    .orThrow();
        }
        return false;
    }

    /**
     * Compute the corresponding {@link Object#hashCode() hash code} for this {@link TreeMap}.
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
                    this.hashCode = hashCode = Objects.hashCode(keyComparator) * 31
                            + hash(entries(objectHashCode(), objectHashCode()), this);
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
        return "TreeMap[" + join(", ", map(into((k, v) -> format("(%s=%s)", k, v)), this)) + "]";
    }

    /**
     * Create a {@link TreeMap} for some key type <code>K</code> and some value type <code>V</code> using the given
     * {@link Comparator} for its keys comparison relation, populated by zero or more given entries.
     *
     * @param keyComparator the {@link Comparator comparison relation}
     * @param entries       the entries
     * @param <K>           the key type
     * @param <V>           the value type
     * @return the {@link TreeMap}
     */
    @SafeVarargs
    public static <K, V> TreeMap<K, V> treeMap(Comparator<? super K> keyComparator, Tuple2<K, V>... entries) {
        return new TreeMap<>(keyComparator, backingTree(keyComparator, entries));
    }

    /**
     * Create a {@link TreeMap} for some {@link Comparable} key type <code>K</code> and some value type <code>V</code>
     * using {@link Comparator#naturalOrder() natural ordering} for its keys, populated by zero or more given entries.
     *
     * @param entries the entries
     * @param <K>     the key type
     * @param <V>     the value type
     * @return the {@link TreeMap}
     */
    @SafeVarargs
    public static <K extends Comparable<? super K>, V> TreeMap<K, V> treeMap(Tuple2<K, V>... entries) {
        return treeMap(naturalOrder(), entries);
    }

    private static <K, V> RedBlackTree<K, V> backingTree(Comparator<? super K> keyComparator, Tuple2<K, V>[] entries) {
        RedBlackTree<K, V> tree = RedBlackTree.empty();
        for (Tuple2<K, V> entry : entries) {
            tree = tree.insert(entry._1(), entry._2(), keyComparator);
        }
        return tree;
    }
}
