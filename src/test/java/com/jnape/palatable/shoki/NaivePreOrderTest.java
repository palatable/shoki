package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import org.junit.Test;

import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn2.All.all;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Span.span;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Unfoldr.unfoldr;
import static com.jnape.palatable.lambda.functions.builtin.fn3.Times.times;
import static com.jnape.palatable.shoki.ImmutableAvlTree.empty;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.isEmpty;
import static testsupport.matchers.IterableMatcher.iterates;

public class NaivePreOrderTest {

    @Test
    public void iteratesNodesPreOrder() {
        Function<ImmutableAvlTree<Integer>, Maybe<Integer>> valueExtractor = ImmutableAvlTree::root;

        assertThat(new NaivePreOrder<>(empty(), valueExtractor), isEmpty());
        assertThat(new NaivePreOrder<>(ImmutableAvlTree.of(1, 2, 3), valueExtractor), iterates(2, 1, 3));
        assertThat(new NaivePreOrder<>(ImmutableAvlTree.of(1, 2, 3, 4, 5, 6), valueExtractor), iterates(4, 2, 1, 3, 5, 6));
    }

    @Test
    public void propertyTest() {
        times(10, __ -> {
            Supplier<Integer> randomValue = new Random()::nextInt;
            int n = 1000;
            Iterable<Integer> elements = unfoldr(xs -> {
                int startSize = xs.size();
                if (startSize == n)
                    return nothing();
                Integer value = -1;
                while (xs.size() == startSize) {
                    value = randomValue.get();
                    xs.add(value);
                }
                return just(tuple(value, xs));
            }, new HashSet<>());
            ImmutableAvlTree<Integer> tree = ImmutableAvlTree.from(elements);
            NaivePreOrder<Integer, ImmutableAvlTree<Integer>> preOrder = new NaivePreOrder<>(tree, ImmutableAvlTree::root);

            Integer root = tree.root().orElseThrow(NoSuchElementException::new);
            return span(x -> x <= root, preOrder).into((lte, gt) -> {
                assertTrue(all(x -> x > root, gt));
                return UNIT;
            });

        }).apply(UNIT);
    }
}