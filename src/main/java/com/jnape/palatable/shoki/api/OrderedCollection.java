package com.jnape.palatable.shoki.api;

import static com.jnape.palatable.lambda.functions.builtin.fn3.ZipWith.zipWith;
import static com.jnape.palatable.lambda.monoid.builtin.And.and;

/**
 * A {@link Collection} that supports stable ordering of the contained elements.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 * @see Stack
 * @see Queue
 */
public interface OrderedCollection<Size extends Number, A> extends Collection<Size, A> {

    /**
     * Produce a new instance of this {@link OrderedCollection} with the elements in the reverse order.
     *
     * @return the reversed {@link OrderedCollection}
     */
    OrderedCollection<Size, A> reverse();

    /**
     * {@inheritDoc}
     */
    @Override
    OrderedCollection<Size, A> tail();

    /**
     * Common {@link EquivalenceRelation}s between {@link OrderedCollection}s.
     */
    final class EquivalenceRelations {
        private EquivalenceRelations() {
        }

        /**
         * An {@link EquivalenceRelation} between two {@link OrderedCollection}s that holds if, and only if, both
         * {@link OrderedCollection}s have equivalent {@link SizeInfo}s and contain the same elements in the same order.
         * <code>O(n)</code>.
         *
         * @param elementEqRel the {@link EquivalenceRelation} to use to compare elements
         * @param <A>          the element type
         * @param <OC>         the {@link OrderedCollection} subtype of the arguments
         * @return the {@link EquivalenceRelation}
         */
        public static <A, OC extends OrderedCollection<?, A>> EquivalenceRelation<OC> sameElementsSameOrder(
                EquivalenceRelation<? super A> elementEqRel) {
            EquivalenceRelation<OC> sameElementsInOrder = (xs, ys) -> and().reduceLeft(zipWith(elementEqRel, xs, ys));
            return Sizable.EquivalenceRelations.<OC>sameSizes().and(sameElementsInOrder);
        }
    }
}
