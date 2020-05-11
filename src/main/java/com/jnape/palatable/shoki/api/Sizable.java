package com.jnape.palatable.shoki.api;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;

/**
 * A generic interface representing a type that can provide information about its size.
 *
 * @see Collection
 */
public interface Sizable {

    /**
     * Returns a {@link SizeInfo} representing any information this type has about its size.
     *
     * @return the information about this type's size
     */
    SizeInfo sizeInfo();

    /**
     * Common {@link EquivalenceRelation}s between {@link Sizable}s.
     */
    final class EquivalenceRelations {

        private EquivalenceRelations() {
        }

        /**
         * An {@link EquivalenceRelation} between two {@link Sizable}s that holds if, and only if, both {@link Sizable}s
         * have equivalent {@link SizeInfo}s. <code>O(1)</code>.
         *
         * @param <S> the {@link Sizable} subtype of the arguments
         * @return the {@link EquivalenceRelation}
         */
        public static <S extends Sizable> EquivalenceRelation<S> sizeInfos() {
            return (xs, ys) -> equivalent(objectEquals(), xs.sizeInfo(), ys.sizeInfo());
        }
    }
}
