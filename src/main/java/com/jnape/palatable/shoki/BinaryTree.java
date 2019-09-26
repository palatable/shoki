package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;

public interface BinaryTree<A, BT extends BinaryTree<A, BT>> extends Tree<A, BT> {

    Maybe<A> root();

    BT left();

    BT right();

    @Override
    default Iterable<BT> branches() {
        return root().fmap(constantly(asList(left(), right()))).orElse(emptyList());
    }
}
