package com.jnape.palatable.shoki.api;

/**
 * An interface offering a mechanism for looking up a value of type <code>A</code> for an index of type
 * <code>Index</code>. Generally, to implement this interface is to suggest that {@link RandomAccess#get(Object) get}
 * is "efficient", where "efficient" is loosely defined as "the fastest available mechanism for accessing a value
 * of type <code>A</code> given an index of type <code>Index</code>".
 * <p>
 * Note that while this loose definition may technically allow for amortized performance of worse than
 * <code>O(1)</code>, an implementing type who's {@link RandomAccess#get(Object) get} implementation cannot offer a
 * performance benefit over merely linearly scanning the underlying structure via successive invocations of other
 * published methods might reconsider implementing this interface.
 *
 * @param <Index> the index type
 * @param <A>     the value type
 */
public interface RandomAccess<Index, A> extends Membership<Index> {

    /**
     * Lookup a value for some given index in an efficient manner.
     *
     * @param index the index
     * @return the value
     */
    A get(Index index);
}
