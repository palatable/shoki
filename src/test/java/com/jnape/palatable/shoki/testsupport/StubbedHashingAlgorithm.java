package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.shoki.HashingAlgorithm;
import com.jnape.palatable.shoki.ImmutableHashMap;

import java.util.Objects;

import static com.jnape.palatable.shoki.ImmutableHashMap.empty;

public final class StubbedHashingAlgorithm<A> implements HashingAlgorithm<A> {
    private final ImmutableHashMap<A, Integer> table;

    private StubbedHashingAlgorithm(ImmutableHashMap<A, Integer> table) {
        this.table = table;
    }

    @Override
    public Integer checkedApply(A a) {
        return table.get(a).orElseGet(() -> Objects.hashCode(a));
    }

    public StubbedHashingAlgorithm<A> stub(A a, Integer hash) {
        return new StubbedHashingAlgorithm<>(table.put(a, hash));
    }

    public static <A> StubbedHashingAlgorithm<A> stubbedHashingAlgorithm() {
        return new StubbedHashingAlgorithm<>(empty());
    }
}
