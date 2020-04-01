package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.api.Natural.NonZero;

import java.math.BigInteger;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.shoki.api.Natural.one;

/**
 * A {@link MultiSet} (also referred to as a <em>Bag</em>) is a {@link Collection} of {@link Tuple2 pairs} of contained
 * elements and their {@link NonZero non-zero} multiplicity in the {@link MultiSet}, supporting {@link RandomAccess}
 * from a possible element to its {@link Natural} multiplicity.
 *
 * @param <A> the element type
 */
public interface MultiSet<A> extends RandomAccess<A, Natural>, Collection<BigInteger, Tuple2<A, NonZero>> {

    /**
     * Add <code>k</code> occurrences of <code>a</code> to this {@link MultiSet}.
     *
     * @param a the element to add
     * @param k the number of occurrences of the element to add
     * @return the updated {@link MultiSet}
     */
    MultiSet<A> add(A a, NonZero k);

    /**
     * Remove <code><em>min</em>(k, {@link MultiSet#get get}(a))</code> occurrences of <code>a</code> from this
     * {@link MultiSet}.
     *
     * @param a the element to remove
     * @param k the number of occurrences of the element to remove
     * @return the updated {@link MultiSet}
     */
    MultiSet<A> remove(A a, NonZero k);

    /**
     * {@link MultiSet#add(Object, NonZero) Add} {@link Natural#one() one} occurrence of <code>a</code> to this
     * {@link MultiSet}.
     *
     * @param a the element to add
     * @return the updated {@link MultiSet}
     * @see MultiSet#add(Object, NonZero)
     */
    default MultiSet<A> add(A a) {
        return add(a, one());
    }

    /**
     * {@link MultiSet#remove(Object, NonZero) Remove}
     * <code><em>min</em>({@link Natural#one() one}, {@link MultiSet#get get}(a))</code> occurrences of <code>a</code>
     * from this {@link MultiSet}.
     *
     * @param a the element to remove
     * @return the updated {@link MultiSet}
     * @see MultiSet#remove(Object, NonZero)
     */
    default MultiSet<A> remove(A a) {
        return remove(a, one());
    }

    /**
     * Remove all occurrences of <code>a</code> from this {@link MultiSet}, such that subsequent invocations of
     * <code>{@link MultiSet#get(Object) get}(a)</code> return {@link Natural#zero() zero}.
     * <p>
     * By default, this method simply {@link MultiSet#remove(Object, NonZero) removes} the
     * {@link MultiSet#get(Object) multiplicity} of <code>a</code> from the {@link MultiSet}, although specific
     * implementations may be able to perform this operation more directly and efficiently.
     *
     * @param a the element to remove
     * @return the updated {@link MultiSet}
     * @see MultiSet#remove(Object, NonZero)
     */
    default MultiSet<A> removeAll(A a) {
        return get(a).match(constantly(this), k -> remove(a, k));
    }

    /**
     * The ({@link Natural possibly zero}) multiplicity (number of occurrences) of <code>a</code> in this
     * {@link MultiSet}.
     *
     * @param a the element for which to lookup the multiplicity
     * @return the multiplicity of a in this {@link MultiSet}
     */
    @Override
    Natural get(A a);

    /**
     * {@inheritDoc}
     *
     * @return this {@link MultiSet} without the {@link MultiSet#head() head} occurrence pair
     */
    @Override
    MultiSet<A> tail();
}
