package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;

import java.util.Comparator;

/**
 * An {@link OrderedCollection} that maintains a sorted ordering over its elements according to the given
 * <code>Ordering</code> type, and offering methods for {@link SortedCollection#sort(Comparator) custom sorting} and
 * retrieving {@link SortedCollection#min() minimal} and {@link SortedCollection#max() maximal} elements.
 *
 * @param <Size>     the known size {@link Number} type
 * @param <A>        the element type
 * @param <Ordering> the type used for the ordering relation
 */
public interface SortedCollection<Size extends Number, A, Ordering> extends OrderedCollection<Size, A> {

    /**
     * If this {@link SortedCollection} is not empty, return {@link Maybe#just(Object) just} the minimum element;
     * otherwise, return {@link Maybe#nothing() nothing}.
     *
     * @return {@link Maybe} the minimum element
     * @see SortedCollection#max()
     */
    Maybe<A> min();

    /**
     * If this {@link SortedCollection} is not empty, return {@link Maybe#just(Object) just} the maximum element;
     * otherwise, return {@link Maybe#nothing() nothing}.
     *
     * @return {@link Maybe} the maximum element
     * @see SortedCollection#min()
     */
    Maybe<A> max();

    /**
     * Sort the elements in this {@link SortedCollection} according to the given {@link Comparator}.
     *
     * @param comparator the {@link Comparator}
     * @return the {@link SortedCollection} sorted according to the given {@link Comparator}
     */
    SortedCollection<Size, A, Ordering> sort(Comparator<? super Ordering> comparator);

    /**
     * {@inheritDoc}
     */
    @Override
    SortedCollection<Size, A, Ordering> reverse();

    /**
     * {@inheritDoc}
     */
    @Override
    SortedCollection<Size, A, Ordering> tail();
}
