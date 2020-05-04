package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.PriorityHeap;

import java.util.Comparator;

/**
 * An {@link OrderedCollection} that offers <em>priority access</em> {@link PriorityCollection#insert(Object) insertion}
 * and {@link PriorityCollection#head() retrieval} semantics.
 *
 * @param <Size>the known size {@link Number} type
 * @param <A>       the element type
 * @see PriorityHeap
 */
public interface PriorityCollection<Size extends Number, A> extends OrderedCollection<Size, A> {

    /**
     * Insert an element into this {@link PriorityCollection} at a position that corresponds to the element's priority,
     * according to the prioritization condition.
     *
     * @param a the element
     * @return the {@link PriorityCollection} with the element inserted in priority order
     */
    PriorityCollection<Size, A> insert(A a);

    /**
     * Prioritize the elements in this {@link PriorityCollection} according to the given {@link Comparator}.
     *
     * @param comparator the {@link Comparator}
     * @return a {@link PriorityCollection} containing the same elements prioritized by the given {@link Comparator}
     */
    PriorityCollection<Size, A> prioritize(Comparator<? super A> comparator);

    /**
     * {@inheritDoc}
     */
    @Override
    PriorityCollection<Size, A> tail();

    /**
     * {@inheritDoc}
     */
    @Override
    PriorityCollection<Size, A> reverse();
}
