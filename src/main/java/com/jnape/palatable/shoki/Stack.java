package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;

/**
 * A LIFO {@link Collection}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 * @param <S>    this specific stack type
 */
public interface Stack<Size extends Number, A, S extends Stack<Size, A, S>> extends Collection<Size, A> {

    /**
     * If this {@link Stack} is not empty, destructure the {@link Stack#head()} and {@link Stack#tail()} into a {@link
     * Tuple2} and wrap it in a {@link Maybe}; otherwise, return {@link Maybe#nothing()}.
     *
     * @return {@link Maybe} the stack's destructured head and tail
     */
    default Maybe<Tuple2<A, S>> shift() {
        return head().fmap(head -> tuple(head, tail()));
    }

    /**
     * Add an element to the top of this {@link Stack}.
     *
     * @param a the element
     * @return this {@link Stack} with the element added to the top
     */
    S unshift(A a);

    /**
     * @inheritDoc
     */
    @Override
    S tail();
}
