package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.ImmutableQueue;
import com.jnape.palatable.shoki.ImmutableStack;

import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;

/**
 * A recursive, homogeneous data structure supporting singly-linked iteration.
 *
 * @param <A> the element type
 * @see ImmutableStack
 * @see ImmutableQueue
 */
public interface Sequence<A> extends Iterable<A> {

    /**
     * If this {@link Sequence} is not empty, return the next element wrapped in {@link Maybe#just}, according to the
     * supported iteration strategy. Otherwise, return {@link Maybe#nothing()};
     *
     * @return {@link Maybe} the next element
     */
    Maybe<A> head();

    /**
     * Return the elements of this {@link Sequence} without the head, if there is one.
     *
     * @return the tail {@link Sequence}
     */
    Sequence<A> tail();

    /**
     * Returns <code>true</code> if calling {@link Sequence#head()} on this {@link Sequence} would return
     * {@link Maybe#nothing()}; <code>false</code>, otherwise.
     * <p>
     * Note that if a subtype supports a more direct implementation than {@link Sequence#head} interactions, the
     * contract <code>isEmpty() == (head() == nothing())</code> must be preserved.
     *
     * @return true if this {@link Sequence} is empty; false, otherwise
     */
    default boolean isEmpty() {
        return head().fmap(constantly(false)).orElse(true);
    }

    /**
     * Return an {@link Iterator} over all of the elements contained in this {@link Sequence}; that is, return an
     * {@link Iterator} that iterates all values of this {@link Sequence} for which {@link Sequence#head()} does not
     * return {@link Maybe#nothing()}.
     * <p>
     * As with {@link Sequence#isEmpty()}, subtypes of {@link Sequence} can very likely provide faster implementations
     * of this method, but should always honor the contract with {@link Sequence#head()}/{@link Sequence#isEmpty()}.
     *
     * @return an {@link Iterator} over the elements in this {@link Sequence}
     */
    @Override
    default Iterator<A> iterator() {
        class NaiveIterator implements Iterator<A> {
            private Sequence<A> sequence;

            private NaiveIterator(Sequence<A> sequence) {
                this.sequence = sequence;
            }

            @Override
            public boolean hasNext() {
                return !sequence.isEmpty();
            }

            @Override
            public A next() {
                A next = sequence.head().orElseThrow(NoSuchElementException::new);
                sequence = sequence.tail();
                return next;
            }
        }

        return new NaiveIterator(this);
    }
}
