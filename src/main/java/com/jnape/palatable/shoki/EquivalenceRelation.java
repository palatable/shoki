package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.functions.specialized.BiPredicate;

import java.util.Comparator;
import java.util.Objects;

public interface EquivalenceRelation<A> extends BiPredicate<A, A> {

    static <A> EquivalenceRelation<A> objectEquals() {
        return Objects::equals;
    }

    static <A> EquivalenceRelation<A> referenceEquality() {
        return (x, y) -> x == y;
    }

    static <A> EquivalenceRelation<A> comparableEquality(Comparator<A> comparator) {
        return (x, y) -> comparator.compare(x, y) == 0;
    }

    static <A extends Comparable<A>> EquivalenceRelation<A> comparableEquality() {
        return comparableEquality(Comparator.<A>naturalOrder());
    }
}
