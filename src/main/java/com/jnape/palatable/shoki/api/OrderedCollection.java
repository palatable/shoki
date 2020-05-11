package com.jnape.palatable.shoki.api;

import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
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
        public static <A, OC extends OrderedCollection<?, A>> EquivalenceRelation<OC> elementsInOrder(
                EquivalenceRelation<? super A> elementEqRel) {
            EquivalenceRelation<OC> elementsInOrder = (xs, ys) -> and().reduceLeft(zipWith(elementEqRel, xs, ys));
            return Sizable.EquivalenceRelations.<OC>sizeInfos().and(elementsInOrder);
        }
    }

    /**
     * Common {@link HashingAlgorithm}s between {@link OrderedCollection}s.
     */
    final class HashingAlgorithms {
        private HashingAlgorithms() {
        }

        /**
         * A {@link HashingAlgorithm} that is derived from the elements contained in an {@link OrderedCollection} and
         * their relative positions in the {@link OrderedCollection}.
         *
         * @param elementHashAlg the {@link HashingAlgorithm} to use for each element
         * @param <A>            the element type
         * @param <OC>           the {@link OrderedCollection} subtype of the argument
         * @return the {@link HashingAlgorithm}
         */
        public static <A, OC extends OrderedCollection<?, A>> HashingAlgorithm<OC> elementsInOrder(
                HashingAlgorithm<? super A> elementHashAlg) {
            return xs -> foldLeft((hash, x) -> (hash * 31) + elementHashAlg.apply(x), 0, xs);
        }
    }
}
