package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn2.Cons;
import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.MultiSet;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Natural.NonZero;
import com.jnape.palatable.shoki.api.SizeInfo.Known;

import java.util.Objects;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Id.id;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.util.Arrays.asList;

/**
 * A {@link MultiSet} that stores elements internally in a {@link HashMap}, supporting the same time/space performance
 * characteristics. As with {@link HashMap}, a {@link HashMultiSet} can be configured upon creation with custom
 * {@link EquivalenceRelation equality} and {@link HashingAlgorithm hashing} semantics.
 *
 * @param <A> the element type
 */
public final class HashMultiSet<A> implements MultiSet<A> {

    private static final HashMultiSet<?> EMPTY_OBJECT_DEFAULTS = new HashMultiSet<>(HashMap.empty());

    private final HashMap<A, NonZero> multiplicityMap;

    private volatile Natural size;

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
     * Create an empty {@link HashMultiSet} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm}.
     * <code>O(1)</code>.
     *
     * @param equivalenceRelation the {@link EquivalenceRelation}
     * @param hashingAlgorithm    the {@link HashingAlgorithm}
     * @param <A>                 the element type
     * @return the empty {@link HashMultiSet}
     */
    public static <A> HashMultiSet<A> empty(EquivalenceRelation<A> equivalenceRelation,
                                            HashingAlgorithm<A> hashingAlgorithm) {
        return new HashMultiSet<>(HashMap.empty(equivalenceRelation, hashingAlgorithm)
        );
    }

    /**
     * The empty singleton {@link HashMultiSet} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively. <code>O(1)</code>.
     *
     * @param <A> the element type
     * @return the empty {@link HashMultiSet}
     */
    @SuppressWarnings("unchecked")
    public static <A> HashMultiSet<A> empty() {
        return (HashMultiSet<A>) EMPTY_OBJECT_DEFAULTS;
    }

    /**
     * Create a new {@link HashMultiSet} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * populated by one or more given entries. <code>O(n)</code>.
     *
     * @param equivalenceRelation the {@link EquivalenceRelation}
     * @param hashingAlgorithm    the {@link HashingAlgorithm}
     * @param a                   the first element
     * @param as                  the rest of the elements
     * @param <A>                 the element type
     * @return the populated {@link HashMultiSet}
     */
    @SafeVarargs
    public static <A> HashMultiSet<A> of(EquivalenceRelation<A> equivalenceRelation,
                                         HashingAlgorithm<A> hashingAlgorithm,
                                         A a, A... as) {
        return foldLeft(HashMultiSet::inc, empty(equivalenceRelation, hashingAlgorithm), Cons.cons(a, asList(as)));
    }

    /**
     * Create a new {@link HashMultiSet} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively, populated by one or more given entries. <code>O(n)</code>.
     *
     * @param a   the first element
     * @param as  the rest of the elements
     * @param <A> the element type
     * @return the populated {@link HashMultiSet}
     */
    @SafeVarargs
    public static <A> HashMultiSet<A> of(A a, A... as) {
        return of(objectEquals(), objectHashCode(), a, as);
    }
}
