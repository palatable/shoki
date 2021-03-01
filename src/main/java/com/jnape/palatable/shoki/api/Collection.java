package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;
import com.jnape.palatable.shoki.api.Value.ComputedAtMostOnce.Known;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;

/**
 * A known-sized {@link Sequence}.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 */
public interface Collection<Size extends Number, A> extends Sequence<A>, Sizable {

    /**
     * The {@link Value} representing the {@link Finite finite} size of this collection.
     *
     * @return the size of this collection
     */
    @Override
    Value<Finite<Size>> sizeInfo();

    /**
     * {@inheritDoc}
     */
    @Override
    Collection<Size, A> tail();

    /**
     * {@inheritDoc}
     * <p>
     * {@link Collection} implementations will attempt to use {@link Collection#sizeInfo()} to determine emptiness if it
     * can be obtained in constant time; otherwise, {@link Collection#head} will be used.
     */
    @Override
    default boolean isEmpty() {
        return sizeInfo().projectA()
                .fmap(Value.ComputedAtMostOnce::evaluation)
                .flatMap(CoProduct2::projectA)
                .fmap(Known::get)
                .fmap(Finite::size)
                .fmap(size -> size.intValue() == 0)
                .orElseGet(() -> head().match(constantly(true), constantly(false)));
    }
}
