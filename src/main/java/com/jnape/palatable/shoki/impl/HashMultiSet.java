package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn1.Downcast;
import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.MultiSet;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Natural.NonZero;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;

import java.util.Iterator;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Id.id;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.Memo.updater;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.finite;
import static com.jnape.palatable.shoki.api.Value.computedOnce;
import static com.jnape.palatable.shoki.impl.HashMap.hashMap;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.concurrent.atomic.AtomicReferenceFieldUpdater.newUpdater;

/**
 * A {@link MultiSet} that stores elements internally in a {@link HashMap}, supporting the same time/space performance
 * characteristics. As with {@link HashMap}, a {@link HashMultiSet} can be configured upon creation with custom
 * {@link EquivalenceRelation equality} and {@link HashingAlgorithm hashing} semantics.
 *
 * @param <A> the element type
 */
public final class HashMultiSet<A> implements MultiSet<A> {

    private static final AtomicReferenceFieldUpdater<HashMultiSet<?>, Natural> SIZE_UPDATER =
            newUpdater(Downcast.<Class<HashMultiSet<?>>, Class<?>>downcast(HashMultiSet.class),
                       Natural.class,
                       "size");


    private static final HashMultiSet<?> EMPTY_OBJECT_DEFAULTS = new HashMultiSet<>(hashMap());

    private final HashMap<A, NonZero> multiplicityMap;

    @SuppressWarnings("unused") private volatile Natural size;

    private HashMultiSet(HashMap<A, NonZero> multiplicityMap) {
        this.multiplicityMap = multiplicityMap;
    }

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public HashMultiSet<A> sum(MultiSet<A> other) {
        return (HashMultiSet<A>) MultiSet.super.sum(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n)</code>.
     */
    @Override
    public HashSet<A> unique() {
        return multiplicityMap.keys();
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public HashMultiSet<A> inc(A a, NonZero k) {
        return new HashMultiSet<>(multiplicityMap.put(a, multiplicityMap.get(a).fmap(k::plus).orElse(k)));
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public HashMultiSet<A> dec(A a, NonZero k) {
        return multiplicityMap.get(a)
                .fmap(n -> new HashMultiSet<>(n.minus(k).orElse(zero())
                                                      .match(zero -> multiplicityMap.remove(a),
                                                             difference -> multiplicityMap.put(a, difference))
                ))
                .orElse(this);
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
    public HashMultiSet<A> remove(A a) {
        return new HashMultiSet<>(multiplicityMap.remove(a));
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public HashMultiSet<A> inc(A a) {
        return (HashMultiSet<A>) MultiSet.super.inc(a);
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public HashMultiSet<A> dec(A a) {
        return (HashMultiSet<A>) MultiSet.super.dec(a);
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
     * Amortized <code>O(1)</code>.
     */
    @Override
    public Finite<Natural> sizeInfo() {
        return finite(computedOnce(updater(this, SIZE_UPDATER),
                                   () -> foldLeft(Natural::plus, (Natural) zero(), multiplicityMap.values())));
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public HashMultiSet<A> tail() {
        return new HashMultiSet<>(multiplicityMap.tail());
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
    public HashMultiSet<A> intersection(MultiSet<A> other) {
        return (HashMultiSet<A>) MultiSet.super.intersection(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public HashMultiSet<A> union(MultiSet<A> other) {
        return (HashMultiSet<A>) MultiSet.super.union(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public HashMultiSet<A> difference(MultiSet<A> other) {
        return (HashMultiSet<A>) MultiSet.super.difference(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public HashMultiSet<A> symmetricDifference(MultiSet<A> other) {
        return (HashMultiSet<A>) MultiSet.super.symmetricDifference(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o)</code>.
     */
    @Override
    public HashMultiSet<A> merge(MultiSet<A> other, Semigroup<Natural> semigroup) {
        return (HashMultiSet<A>) MultiSet.super.merge(other, semigroup);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Iterator<Tuple2<A, NonZero>> iterator() {
        return multiplicityMap.iterator();
    }

    /**
     * Returns <code>true</code> if <code>other</code> is a {@link HashMultiSet} and
     * {@link EquivalenceRelation equivalent} to this {@link HashMultiSet} according to the underlying
     * {@link HashMap HashMaps}; <code>false</code> otherwise. <code>O(n)</code>.
     *
     * @param other the {@link Object} to check for equality
     * @return the equality outcome
     * @see HashMap#equals(Object)
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof HashMultiSet<?> && multiplicityMap.equals(((HashMultiSet<?>) other).multiplicityMap);
    }

    /**
     * Compute the corresponding {@link Object#hashCode() hash code} for this {@link HashMultiSet}.
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
        return "HashMultiSet["
                + join(", ", map(into((a, k) -> format("(%s * %s)", a, k.bigIntegerValue())), this)) + "]";
    }

    /**
     * Create a {@link HashMultiSet} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * populated by zero or more given entries. <code>O(n)</code>.
     *
     * @param equivalenceRelation the {@link EquivalenceRelation}
     * @param hashingAlgorithm    the {@link HashingAlgorithm}
     * @param as                  the elements
     * @param <A>                 the element type
     * @return the {@link HashMultiSet}
     */
    @SafeVarargs
    public static <A> HashMultiSet<A> hashMultiSet(EquivalenceRelation<? super A> equivalenceRelation,
                                                   HashingAlgorithm<? super A> hashingAlgorithm,
                                                   A... as) {
        return hashMultiSet(new HashMultiSet<>(hashMap(equivalenceRelation, hashingAlgorithm)), as);
    }

    private static <A> HashMultiSet<A> hashMultiSet(HashMultiSet<A> hashMultiSet, A[] as) {
        for (A a : as)
            hashMultiSet = hashMultiSet.inc(a);
        return hashMultiSet;
    }

    /**
     * Create a {@link HashMultiSet} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively, populated by zero or more given entries. <code>O(n)</code>.
     *
     * @param as  the elements
     * @param <A> the element type
     * @return the {@link HashMultiSet}
     */
    @SafeVarargs
    public static <A> HashMultiSet<A> hashMultiSet(A... as) {
        @SuppressWarnings("unchecked")
        HashMultiSet<A> emptyObjectDefaults = (HashMultiSet<A>) EMPTY_OBJECT_DEFAULTS;
        return hashMultiSet(emptyObjectDefaults, as);
    }
}
