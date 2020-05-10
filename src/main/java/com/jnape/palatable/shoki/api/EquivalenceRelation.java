package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.lambda.functions.specialized.BiPredicate;
import com.jnape.palatable.lambda.functor.Applicative;

import java.util.Comparator;
import java.util.Objects;

import static com.jnape.palatable.lambda.functions.Fn2.curried;
import static java.util.Comparator.naturalOrder;

/**
 * An {@link EquivalenceRelation equivalence relation} between two terms of some type <code>A</code> satisfies the
 * following characteristics:
 * <ul>
 * <li>Reflexivity: <code>eq.apply(a, a) == true</code></li>
 * <li>Symmetry: <code>eq.apply(a, b) == eq.apply(b, a)</code></li>
 * <li>Transitivity: <code>eq.apply(a, b) &amp;&amp; eq.apply(b, c) == eq.apply(a, c)</code></li>
 * <li>Consistency: <code>eq.apply(a, b) == eq.apply(a, b)</code></li>
 * <li>Nullability: <code>eq.apply(a, null) == (a == null)</code></li>
 * </ul>
 *
 * @param <A> the type to equate
 */
public interface EquivalenceRelation<A> extends BiPredicate<A, A> {

    /**
     * {@inheritDoc}
     */
    @Override
    default EquivalenceRelation<A> and(BiPredicate<? super A, ? super A> other) {
        return BiPredicate.super.and(other)::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default EquivalenceRelation<A> flip() {
        return BiPredicate.super.flip()::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default <D> EquivalenceRelation<A> discardR(Applicative<D, Fn1<A, ?>> appB) {
        return BiPredicate.super.discardR(appB)::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default EquivalenceRelation<A> or(BiPredicate<? super A, ? super A> other) {
        return BiPredicate.super.or(other)::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default EquivalenceRelation<A> negate() {
        return BiPredicate.super.negate()::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default EquivalenceRelation<A> local(Fn1<? super A, ? extends A> fn) {
        return curried(BiPredicate.super.local(fn))::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default EquivalenceRelation<A> censor(Fn1<? super A, ? extends A> fn) {
        return curried(BiPredicate.super.censor(fn))::apply;
    }

    /**
     * An {@link EquivalenceRelation} implemented in terms of {@link Objects#equals(Object, Object)}.
     *
     * @param <A> the type to equate
     * @return an {@link EquivalenceRelation} implemented in terms of {@link Objects#equals(Object, Object)}
     */
    static <A> EquivalenceRelation<A> objectEquals() {
        return Objects::equals;
    }

    /**
     * An {@link EquivalenceRelation} implemented in terms of reference equality (<code>==</code>).
     *
     * @param <A> the type to equate
     * @return an {@link EquivalenceRelation} implemented in terms of reference equality (<code>==</code>)
     */
    static <A> EquivalenceRelation<A> referenceEquals() {
        return (x, y) -> x == y;
    }

    /**
     * An {@link EquivalenceRelation} implemented in terms of the given {@link Comparator Comparator's}
     * {@link Comparator#compare(Object, Object) compare} method.
     *
     * @param comparator the {@link Comparator}
     * @param <A>        the type to equate
     * @return an {@link EquivalenceRelation} implemented in terms of the given {@link Comparator Comparator's}
     * {@link Comparator#compare(Object, Object) compare} method.
     */
    static <A> EquivalenceRelation<A> comparablyEquals(Comparator<? super A> comparator) {
        return (x, y) -> comparator.compare(x, y) == 0;
    }

    /**
     * An {@link EquivalenceRelation} implemented in terms of the {@link Comparable} type's
     * {@link Comparable#compareTo(Object) compareTo} method.
     *
     * @param <A> the type to equate
     * @return an {@link EquivalenceRelation} implemented in terms of the {@link Comparable} type's
     * {@link Comparable#compareTo(Object) compareTo} method.
     */
    static <A extends Comparable<? super A>> EquivalenceRelation<A> comparablyEquals() {
        return comparablyEquals(naturalOrder());
    }

    /**
     * Contextualized equality given an {@link EquivalenceRelation} and two values <code>x</code> and <code>y</code>
     * that it compares.
     *
     * @param <A> the value type
     * @param x   the first value to compare for equality
     * @param y   the second value to compare for equality
     * @param eq  the {@link EquivalenceRelation}
     * @return true if the values are equal according to the {@link EquivalenceRelation}; false otherwise
     */
    static <A> boolean equivalent(A x, A y, EquivalenceRelation<? super A> eq) {
        return eq.apply(x, y);
    }
}
