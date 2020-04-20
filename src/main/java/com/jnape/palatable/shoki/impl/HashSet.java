package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn2.Cons;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Set;
import com.jnape.palatable.shoki.api.SizeInfo.Known;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn2.GT.gt;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.LT.lt;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static java.lang.String.join;
import static java.util.Arrays.asList;

/**
 * A {@link Set} that stores elements internally in a {@link HashMap}, supporting the same time/space performance
 * characteristics. As with {@link HashMap}, a {@link HashSet} can be configured upon creation with custom
 * {@link EquivalenceRelation equality} and {@link HashingAlgorithm hashing} semantics.
 *
 * @param <A> the element type
 * @see HashMap
 */
public final class HashSet<A> implements Set<Natural, A> {

    private static final HashSet<?> DEFAULT_EMPTY = new HashSet<>(HashMap.empty());

    private final HashMap<A, Unit> map;

    private HashSet(HashMap<A, Unit> map) {
        this.map = map;
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public HashSet<A> add(A a) {
        return new HashSet<>(map.put(a, UNIT));
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    public HashSet<A> remove(A a) {
        return new HashSet<>(map.remove(a));
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
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public Maybe<A> head() {
        return map.head().fmap(Tuple2::_1);
    }

    /**
     * {@inheritDoc}
     * <code>O(1)</code>.
     */
    @Override
    public HashSet<A> tail() {
        return new HashSet<>(map.tail());
    }

    /**
     * {@inheritDoc}
     * <code>True</code> if <code>a</code> is a member of this {@link Set}; <code>false</code> otherwise. Amortized
     * <code>O(1)</code>.
     */
    @Override
    public boolean contains(A a) {
        return map.contains(a);
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
     * <code>O(max(n, o))</code>.
     */
    @Override
    public HashSet<A> intersection(Set<Natural, A> other) {
        return (lt(other.sizeInfo().getSize(), sizeInfo().getSize()) ? tuple(this, other) : tuple(other, this))
                .into((source, filter) -> foldLeft((i, x) -> filter.contains(x) ? i.add(x) : i, empty(), source));
    }

    /**
     * {@inheritDoc}
     * <code>O(min(n, o))</code>.
     */
    @Override
    public HashSet<A> union(Set<Natural, A> other) {
        return (other instanceof HashSet<?> && gt(sizeInfo().getSize(), other.sizeInfo().getSize())
                ? tuple((HashSet<A>) other, this)
                : tuple(this, other))
                .into(foldLeft(HashSet::add));
    }

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public HashSet<A> difference(Set<Natural, A> other) {
        return foldLeft(HashSet<A>::remove, this, other);
    }

    /**
     * {@inheritDoc}
     * <code>O(n + o + min(n, o))</code>.
     */
    @Override
    public HashSet<A> symmetricDifference(Set<Natural, A> other) {
        return (HashSet<A>) Set.super.symmetricDifference(other);
    }

    /**
     * Determine if <code>other</code> is a {@link HashSet} with the same elements as this {@link HashSet} (according to
     * the underlying {@link HashMap}). <code>O(n)</code>
     *
     * @param other the {@link Object} to check for equality
     * @return the equality outcome
     * @see HashMap#equals(Object)
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof HashSet<?> && map.equals(((HashSet<?>) other).map);
    }

    /**
     * Compute the corresponding {@link Object#hashCode() hash code} for this {@link HashSet}.
     * Amortized <code>O(1)</code>.
     *
     * @return the hash code
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
        return "HashSet[" + join(", ", map(into((e, __) -> e.toString()), map)) + ']';
    }

    /**
     * Create an empty {@link HashSet} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm}.
     * <code>O(1)</code>.
     *
     * @param equivalenceRelation the {@link EquivalenceRelation}
     * @param hashingAlgorithm    the {@link HashingAlgorithm}
     * @param <A>                 the element type
     * @return the empty {@link HashSet}
     */
    public static <A> HashSet<A> empty(EquivalenceRelation<A> equivalenceRelation,
                                       HashingAlgorithm<A> hashingAlgorithm) {
        return new HashSet<>(HashMap.empty(equivalenceRelation, hashingAlgorithm));
    }

    /**
     * The empty singleton {@link HashSet} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively. <code>O(1)</code>.
     *
     * @param <A> the element type
     * @return the empty {@link HashSet}
     */
    @SuppressWarnings("unchecked")
    public static <A> HashSet<A> empty() {
        return (HashSet<A>) DEFAULT_EMPTY;
    }

    /**
     * Create a new {@link HashSet} using the given {@link EquivalenceRelation} and {@link HashingAlgorithm}, populated
     * by one or more given entries. <code>O(n)</code>.
     *
     * @param equivalenceRelation the {@link EquivalenceRelation}
     * @param hashingAlgorithm    the {@link HashingAlgorithm}
     * @param a                   the first element
     * @param as                  the rest of the elements
     * @param <A>                 the element type
     * @return the populated {@link HashSet}
     */
    @SafeVarargs
    public static <A> HashSet<A> of(EquivalenceRelation<A> equivalenceRelation, HashingAlgorithm<A> hashingAlgorithm,
                                    A a, A... as) {
        return foldLeft(HashSet::add, HashSet.empty(equivalenceRelation, hashingAlgorithm), Cons.cons(a, asList(as)));
    }

    /**
     * Create a new {@link HashSet} using {@link Objects#equals(Object, Object) Object equality} and
     * {@link Objects#hashCode(Object) Object hashCode} as the {@link EquivalenceRelation} and {@link HashingAlgorithm},
     * respectively, populated by one or more given entries. <code>O(n)</code>.
     *
     * @param a   the first element
     * @param as  the rest of the elements
     * @param <A> the element type
     * @return the populated {@link HashSet}
     */
    @SafeVarargs
    public static <A> HashSet<A> of(A a, A... as) {
        return of(objectEquals(), objectHashCode(), a, as);
    }
}
