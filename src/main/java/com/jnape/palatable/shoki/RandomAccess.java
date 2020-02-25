package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;

/**
 * An interface indicating that the implementing type supports an "efficient" mapping from the index type
 * <code>Index</code> to the value type <code>A</code>, where "efficient" is loosely defined to be the fastest available
 * mechanism for accessing a value of type <code>A</code> given an index of type <code>Index</code>.
 * <p>
 * While this loose definition may technically allow for amortized performance of worse than <code>O(1)</code>, an
 * implementing type who's {@link RandomAccess#get(Object)} implementation cannot offer a performance benefit over
 * merely linearly scanning the underlying structure via successive invocations of other published methods should be
 * appropriately treated with skepticism.
 *
 * @param <Index> the index type
 * @param <A>     the value type
 */
public interface RandomAccess<Index, A> extends Membership<Index> {

    /**
     * Lookup a value for some given index in an efficient manner.
     *
     * @param index the index
     * @return {@link Maybe} the value
     */
    Maybe<A> get(Index index);

    /**
     * {@inheritDoc}
     */
    @Override
    default boolean contains(Index index) {
        return get(index).match(constantly(false), constantly(true));
    }
}
