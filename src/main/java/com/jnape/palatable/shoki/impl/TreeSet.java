package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Set;
import com.jnape.palatable.shoki.api.SizeInfo.Known;
import com.jnape.palatable.shoki.api.SortedCollection;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.shoki.impl.TreeMap.treeMap;
import static java.lang.String.join;

/**
 * A {@link Set} and {@link SortedCollection} that stores elements internally in a {@link TreeMap}, supporting the same
 * time/space performance characteristics. As with {@link TreeMap}, a {@link TreeSet} can be configured upon creation
 * with a custom {@link Comparator comparison relation}.
 *
 * @param <A> the element type
 * @see TreeMap
 */
public final class TreeSet<A> implements Set<Natural, A>, SortedCollection<Natural, A, A> {
    private final TreeMap<A, Unit> map;

    private TreeSet(TreeMap<A, Unit> map) {
        this.map = map;
    }

    /**
     * The {@link Comparator} used for the elements stored in this {@link TreeSet}.
     *
     * @return the element {@link Comparator}
     */
    public Comparator<A> comparator() {
        return map.keyComparator();
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public boolean isEmpty() {
        return map.isEmpty();
    }

    /**
     * {@inheritDoc}
     * <code>True</code> if <code>a</code> is a member of this {@link Set}; <code>false</code> otherwise.
     * <code>O(log2n)</code>.
     */
    @Override
    public boolean contains(A a) {
        return map.contains(a);
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public TreeSet<A> add(A a) {
        return new TreeSet<>(map.put(a, UNIT));
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public TreeSet<A> remove(A a) {
        return new TreeSet<>(map.remove(a));
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<A> head() {
        return map.head().fmap(Tuple2::_1);
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public TreeSet<A> tail() {
        return new TreeSet<>(map.tail());
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n * o)</code>.
     */
    @Override
    public TreeSet<A> intersection(Set<Natural, A> other) {
        return (TreeSet<A>) Set.super.intersection(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n * o)</code>.
     */
    @Override
    public TreeSet<A> union(Set<Natural, A> other) {
        return (TreeSet<A>) Set.super.union(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n * o)</code>.
     */
    @Override
    public TreeSet<A> difference(Set<Natural, A> other) {
        return (TreeSet<A>) Set.super.difference(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o + min(n, o))</code>.
     */
    @Override
    public TreeSet<A> symmetricDifference(Set<Natural, A> other) {
        return (TreeSet<A>) Set.super.symmetricDifference(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<A> min() {
        return map.min().fmap(Tuple2::_1);
    }

    /**
     * {@inheritDoc}
     * <code>O(log2n)</code>.
     */
    @Override
    public Maybe<A> max() {
        return map.max().fmap(Tuple2::_1);
    }

    /**
     * {@inheritDoc}
     * <code>O(nlogn)</code>.
     */
    @Override
    public TreeSet<A> sort(Comparator<? super A> comparator) {
        return new TreeSet<>(map.sort(comparator));
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public TreeSet<A> reverse() {
        return new TreeSet<>(map.reverse());
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public Known<Natural> sizeInfo() {
        return map.sizeInfo();
    }

    /**
     * Produce an {@link Iterator} of the elements in this {@link TreeSet} according to the
     * {@link TreeMap#iterator() iteration} of the backing {@link TreeMap}.
     *
     * @return the {@link Iterator}
     */
    @Override
    public Iterator<A> iterator() {
        return map(Tuple2::_1, map).iterator();
    }

    /**
     * Determine if <code>other</code> is a {@link TreeSet} with the same elements as this {@link TreeSet} (according
     * to the underlying {@link TreeMap}). <code>O(n * log2o)</code>.
     *
     * @param other the {@link Object} to check for equality
     * @return the equality outcome
     * @see TreeMap#equals(Object)
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof TreeSet<?> && Objects.equals(map, ((TreeSet<?>) other).map);
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public int hashCode() {
        return map.hashCode();
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public String toString() {
        return "TreeSet[" + join(", ", map(into((e, __) -> e.toString()), map)) + ']';
    }

    /**
     * Create a {@link TreeSet} for some element type <code>A</code> using the given {@link Comparator}, populated by
     * zero or more given entries. <code>O(n * log2n)</code>.
     *
     * @param comparator the ordering relation
     * @param as         the elements
     * @param <A>        the element type
     * @return the {@link TreeSet}
     */
    @SafeVarargs
    public static <A> TreeSet<A> treeSet(Comparator<? super A> comparator, A... as) {
        return new TreeSet<>(backingTreeMap(treeMap(comparator), as));
    }

    /**
     * Create a {@link TreeSet} for some {@link Comparable} element type <code>A</code> using
     * {@link Comparator#naturalOrder() natural ordering} for its elements, populated by zero or more given entries.
     * <code>O(n * log2n)</code>.
     *
     * @param as  the elements
     * @param <A> the element type
     * @return the {@link TreeSet}
     */
    @SafeVarargs
    public static <A extends Comparable<? super A>> TreeSet<A> treeSet(A... as) {
        return new TreeSet<>(backingTreeMap(treeMap(), as));
    }

    private static <A> TreeMap<A, Unit> backingTreeMap(TreeMap<A, Unit> treeMap, A[] as) {
        for (A a : as)
            treeMap = treeMap.put(a, UNIT);
        return treeMap;
    }
}
