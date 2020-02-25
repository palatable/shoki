package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;

public final class ImmutableHashSet<A> implements Collection<Integer, A>, Membership<A> {

    private static final ImmutableHashSet<?> DEFAULT_EMPTY = new ImmutableHashSet<>(ImmutableHashMap.empty());

    private final ImmutableHashMap<A, Unit> table;

    private ImmutableHashSet(ImmutableHashMap<A, Unit> table) {
        this.table = table;
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
}
