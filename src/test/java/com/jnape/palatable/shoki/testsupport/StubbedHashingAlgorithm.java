package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.shoki.HashArrayMappedTrie;
import com.jnape.palatable.shoki.api.HashingAlgorithm;

import java.util.Objects;

import static com.jnape.palatable.shoki.HashArrayMappedTrie.empty;

public final class StubbedHashingAlgorithm<A> implements HashingAlgorithm<A> {
    private final HashArrayMappedTrie<A, Integer> table;

    private StubbedHashingAlgorithm(HashArrayMappedTrie<A, Integer> table) {
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
