package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.builtin.fn2.Cons;
import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.api.Set;

import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static java.util.Arrays.asList;

public final class ImmutableHashSet<A> implements Set<Integer, A> {

    private static final ImmutableHashSet<?> DEFAULT_EMPTY = new ImmutableHashSet<>(ImmutableHashMap.empty());

    private final ImmutableHashMap<A, Unit> table;

    private ImmutableHashSet(ImmutableHashMap<A, Unit> table) {
        this.table = table;
    }

    public ImmutableHashSet<A> add(A a) {
        return new ImmutableHashSet<>(table.put(a, UNIT));
    }

    @Override
    public SizeInfo.Known<Integer> sizeInfo() {
        return table.sizeInfo();
    }

    @Override
    public Maybe<A> head() {
        return table.head().fmap(Tuple2::_1);
    }

    @Override
    public ImmutableHashSet<A> tail() {
        return new ImmutableHashSet<>(table.tail());
    }

    @Override
    public boolean contains(A a) {
        return table.contains(a);
    }

    @Override
    public boolean isEmpty() {
        return table.isEmpty();
    }

    public static <A> ImmutableHashSet<A> empty(EquivalenceRelation<A> equivalenceRelation,
                                                HashingAlgorithm<A> hashingAlgorithm) {
        return new ImmutableHashSet<>(ImmutableHashMap.empty(equivalenceRelation, hashingAlgorithm));
    }

    @SuppressWarnings("unchecked")
    public static <A> ImmutableHashSet<A> empty() {
        return (ImmutableHashSet<A>) DEFAULT_EMPTY;
    }

    @SafeVarargs
    public static <A> ImmutableHashSet<A> of(A a, A... as) {
        return foldLeft(ImmutableHashSet::add, ImmutableHashSet.empty(), Cons.cons(a, asList(as)));
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ImmutableHashSet<?> &&
            ((ImmutableHashSet<?>) obj).table.equals(table);
    }

    @Override
    public int hashCode() {
        return table.hashCode();
    }

    @Override
    public String toString() {
        return "ImmutableHashSet[" +
            String.join(", ", map(into((e, __) -> e.toString()), table)) +
            ']';
    }
}
