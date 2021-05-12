package com.jnape.palatable.shoki.api;

import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.monoid.builtin.And.and;

/**
 * A {@link Set} is a {@link Collection} of distinct elements with a {@link Membership} capability for determining if
 * an element is a member of the {@link Set}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public interface Set<Size extends Number, A> extends Collection<Size, A>, Membership<A> {

    /**
     * Include <code>a</code> as a member of this {@link Set}.
     *
     * @param a the element
     * @return the updated {@link Set}
     */
    Set<Size, A> add(A a);

    /**
     * Exclude <code>a</code> as a member of this {@link Set}.
     *
     * @param a the element
     * @return the updated {@link Set}
     */
    Set<Size, A> remove(A a);

    /**
     * {@inheritDoc}
     *
     * @return this {@link Set} without the {@link Set#head() head} element's membership
     */
    @Override
    Set<Size, A> tail();

    /**
     * Grant each element in <code>collection</code> membership in this <code>{@link Set}</code> and return the updated
     * {@link Set}.
     *
     * @param collection the {@link Collection} to add elements from
     * @return this {@link Set} after including all elements of collection
     */
    default Set<Size, A> addAll(Collection<Size, A> collection) {
        return foldLeft(Set<Size, A>::add, this, collection);
    }

    /**
     * The <a href="https://en.wikipedia.org/wiki/Intersection_(set_theory)" target="_new">intersection</a> of two
     * {@link Set Sets} <code>xs</code> and <code>ys</code> is the {@link Set} of those elements present in both
     * <code>xs</code> and <code>ys</code>.
     *
     * @param other the {@link Set} to intersect this {@link Set} with
     * @return the intersection {@link Set}
     */
    default Set<Size, A> intersection(Set<Size, A> other) {
        return foldLeft((intersection, a) -> other.contains(a) ? intersection : intersection.remove(a), this, this);
    }

    /**
     * The <a href="https://en.wikipedia.org/wiki/Union_(set_theory)" target="_new">union</a> of two {@link Set Sets}
     * <code>xs</code> and <code>ys</code> is the {@link Set} of those elements present in either <code>xs</code> or
     * <code>ys</code>.
     *
     * @param other the {@link Set} to union this {@link Set} with
     * @return the union {@link Set}
     * @see Set#addAll(Collection)
     */
    default Set<Size, A> union(Set<Size, A> other) {
        return addAll(other);
    }

    /**
     * The <a href="https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement"
     * target="_new">difference</a> (also known as "relative complement") of two {@link Set Sets} <code>xs</code> and
     * <code>ys</code> is the {@link Set} of those elements present in <code>xs</code> that are not also present in
     * <code>ys</code>.
     *
     * @param other the {@link Set} to subtract from this {@link Set}
     * @return the difference {@link Set}
     */
    default Set<Size, A> difference(Set<Size, A> other) {
        return foldLeft(Set<Size, A>::remove, this, other);
    }

    /**
     * The <a href="https://en.wikipedia.org/wiki/Symmetric_difference" target="_new">symmetric difference</a> of two
     * {@link Set Sets} <code>xs</code> and <code>ys</code> is the {@link Set} of those elements that appear in either
     * <code>xs</code> or <code>ys</code> but do not appear in both, and has denotational equivalence with the
     * {@link Set#union(Set) union} of <code>xs.{@link Set#difference(Set) difference}(ys)</code> and
     * <code>ys.{@link Set#difference(Set) difference}(xs)</code>.
     *
     * @param other the {@link Set} to symmetrically difference this {@link Set} with
     * @return the symmetric difference {@link Set}
     */
    default Set<Size, A> symmetricDifference(Set<Size, A> other) {
        return difference(other).union(other.difference(this));
    }

    /**
     * Common {@link EquivalenceRelation}s between {@link Set}s.
     */
    final class EquivalenceRelations {

        private EquivalenceRelations() {
        }

        /**
         * An {@link EquivalenceRelation} between two {@link Set}s that holds if, and only if, both {@link Set}s have
         * the same elements. <code>O(n)</code>.
         *
         * @param <A> the element type
         * @param <S> the {@link Set} subtype of the arguments
         * @return the {@link EquivalenceRelation}
         */
        public static <A, S extends Set<?, A>> EquivalenceRelation<S> sameElements() {
            EquivalenceRelation<S> sameElements = (xs, ys) -> and().foldMap(ys::contains, xs);
            return Sizable.EquivalenceRelations.<S>sizeInfos().and(sameElements);
        }
    }
}
