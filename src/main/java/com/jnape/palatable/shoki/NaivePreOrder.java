package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.functions.builtin.fn2.Cons;

import java.util.Iterator;
import java.util.function.Function;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Flatten.flatten;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Id.id;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;

public final class NaivePreOrder<A, T extends Tree<A, T>> implements DepthFirst<A> {

    private final T                     tree;
    private final Function<T, Maybe<A>> valueExtractor;

    public NaivePreOrder(T tree, Function<T, Maybe<A>> valueExtractor) {
        this.valueExtractor = valueExtractor;
        this.tree = tree;
    }

    @Override
    public Iterator<A> iterator() {
        return valueExtractor.apply(tree)
                .fmap(Cons::cons)
                .orElse(id()).apply(flatten(map(subTree -> new NaivePreOrder<>(subTree, valueExtractor),
                                                tree.branches()))).iterator();
    }
}
