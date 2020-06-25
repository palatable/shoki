package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.MultiSet;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Natural.NonZero;
import com.jnape.palatable.shoki.api.SizeInfo.Known;
import com.jnape.palatable.shoki.api.SortedCollection;

import java.util.Comparator;
import java.util.Iterator;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Id.id;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.TreeMap.treeMap;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Comparator.naturalOrder;

/**
 * A {@link MultiSet} and {@link SortedCollection} that stores elements internally in a {@link TreeMap}, supporting the
 * same time/space performance characteristics. As with {@link TreeMap}, a {@link TreeMultiSet} can be configured upon
 * creation with a custom {@link Comparator comparison relation}.
 *
 * @param <A> the element type
 */
public final class TreeMultiSet<A> implements MultiSet<A>, SortedCollection<Natural, Tuple2<A, NonZero>, A> {

    private final TreeMap<A, NonZero> multiplicityMap;

    private volatile Natural size;

    private TreeMultiSet(TreeMap<A, NonZero> multiplicityMap) {
        this.multiplicityMap = multiplicityMap;
    }

    /**
     * The {@link Comparator} used for the elements stored in this {@link TreeMultiSet}.
     *
     * @return the element {@link Comparator}
     */
    public Comparator<A> comparator() {
        return multiplicityMap.keyComparator();
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public TreeMultiSet<A> inc(A a, NonZero k) {
        return new TreeMultiSet<>(multiplicityMap.put(a, multiplicityMap.get(a).fmap(k::plus).orElse(k)));
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public TreeMultiSet<A> dec(A a, NonZero k) {
        return multiplicityMap.get(a)
                .fmap(n -> new TreeMultiSet<>(n.minus(k).orElse(zero())
                                                      .match(zero -> multiplicityMap.remove(a),
                                                             difference -> multiplicityMap.put(a, difference))
                ))
                .orElse(this);
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public TreeSet<A> unique() {
        return multiplicityMap.keys();
    }

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public TreeMultiSet<A> sum(MultiSet<A> other) {
        return (TreeMultiSet<A>) MultiSet.super.sum(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public boolean isEmpty() {
        return multiplicityMap.isEmpty();
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public TreeMultiSet<A> remove(A a) {
        return new TreeMultiSet<>(multiplicityMap.remove(a));
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public TreeMultiSet<A> inc(A a) {
        return (TreeMultiSet<A>) MultiSet.super.inc(a);
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public TreeMultiSet<A> dec(A a) {
        return (TreeMultiSet<A>) MultiSet.super.dec(a);
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public Natural get(A a) {
        return multiplicityMap.get(a).match(constantly(zero()), id());
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<Tuple2<A, NonZero>> min() {
        return multiplicityMap.min();
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<Tuple2<A, NonZero>> max() {
        return multiplicityMap.max();
    }

    /**
     * {@inheritDoc}
     * <code>O(nlogn)</code>.
     */
    @Override
    public TreeMultiSet<A> sort(Comparator<? super A> comparator) {
        return new TreeMultiSet<>(multiplicityMap.sort(comparator));
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public TreeMultiSet<A> reverse() {
        return new TreeMultiSet<>(multiplicityMap.reverse());
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public Known<Natural> sizeInfo() {
        Natural size = this.size;
        if (size == null) {
            synchronized (this) {
                size = this.size;
                if (size == null) {
                    this.size = size = foldLeft(Natural::plus, (Natural) zero(), multiplicityMap.values());
                }
            }
        }
        return known(size);
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public TreeMultiSet<A> tail() {
        return new TreeMultiSet<>(multiplicityMap.tail());
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public Maybe<Tuple2<A, NonZero>> head() {
        return multiplicityMap.head();
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public TreeMultiSet<A> intersection(MultiSet<A> other) {
        return (TreeMultiSet<A>) MultiSet.super.intersection(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public TreeMultiSet<A> union(MultiSet<A> other) {
        return (TreeMultiSet<A>) MultiSet.super.union(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public TreeMultiSet<A> difference(MultiSet<A> other) {
        return (TreeMultiSet<A>) MultiSet.super.difference(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public TreeMultiSet<A> symmetricDifference(MultiSet<A> other) {
        return (TreeMultiSet<A>) MultiSet.super.symmetricDifference(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public TreeMultiSet<A> merge(MultiSet<A> other, Semigroup<Natural> semigroup) {
        return (TreeMultiSet<A>) MultiSet.super.merge(other, semigroup);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Iterator<Tuple2<A, NonZero>> iterator() {
        return multiplicityMap.iterator();
    }

    /**
     * Returns <code>true</code> if <code>other</code> is a {@link TreeMultiSet} and
     * {@link EquivalenceRelation equivalent} to this {@link TreeMultiSet} according to the underlying
     * {@link TreeMap}; <code>false</code> otherwise. <code>O(n * log2o)</code>.
     *
     * @param other the {@link Object} to check for equality
     * @return the equality outcome
     * @see TreeMap#equals(Object)
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof TreeMultiSet<?> && multiplicityMap.equals(((TreeMultiSet<?>) other).multiplicityMap);
    }

    /**
     * Compute the corresponding {@link Object#hashCode() hash code} for this {@link TreeMultiSet}.
     * Amortized <code>O(1)</code>.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return multiplicityMap.hashCode();
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public String toString() {
        return "TreeMultiSet["
                + join(", ", map(into((a, k) -> format("(%s * %s)", a, k.bigIntegerValue())), this)) + "]";
    }

    /**
     * Create a {@link TreeMultiSet} for some element type <code>A</code> using the given {@link Comparator}, populated
     * by zero or more given entries. <code>O(n * log2n)</code>.
     *
     * @param comparator the {@link Comparator}
     * @param as         the elements
     * @param <A>        the element type
     * @return the {@link TreeMultiSet}
     */
    @SafeVarargs
    public static <A> TreeMultiSet<A> treeMultiSet(Comparator<? super A> comparator, A... as) {
        return treeMultiSet(new TreeMultiSet<>(treeMap(comparator)), as);
    }

    /**
     * Create a {@link TreeMultiSet} for some {@link Comparable} element type <code>A</code> using
     * {@link Comparator#naturalOrder() natural ordering} for its elements, populated by zero or more given entries.
     * <code>O(n * log2n)</code>.
     *
     * @param as  the elements
     * @param <A> the element type
     * @return the {@link TreeMultiSet}
     */
    @SafeVarargs
    public static <A extends Comparable<? super A>> TreeMultiSet<A> treeMultiSet(A... as) {
        return treeMultiSet(naturalOrder(), as);
    }

    private static <A> TreeMultiSet<A> treeMultiSet(TreeMultiSet<A> treeMultiSet, A[] as) {
        for (A a : as)
            treeMultiSet = treeMultiSet.inc(a);
        return treeMultiSet;
    }
}
