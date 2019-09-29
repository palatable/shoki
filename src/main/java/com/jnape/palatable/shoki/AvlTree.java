package com.jnape.palatable.shoki;

import static com.jnape.palatable.lambda.functions.builtin.fn2.Eq.eq;
import static com.jnape.palatable.lambda.functions.builtin.fn2.LT.lt;

public interface AvlTree<A extends Comparable<A>, Witness extends com.jnape.palatable.shoki.AvlTree<A, Witness>>
        extends BinarySearchTree<A, Witness> {

    Witness left();

    Witness right();

    @Override
    default boolean contains(A a) {
        return root()
                .fmap(value -> eq(value, a) || (lt(value, a) ? left().contains(a) : right().contains(a)))
                .orElse(false);
    }
}
