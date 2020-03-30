package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.impl.HashMap;

import java.util.Objects;

import static com.jnape.palatable.shoki.impl.HashMap.empty;

/**
 * A {@link HashingAlgorithm} that can be used to stub custom hash results for values.
 *
 * @param <A> the type to hash
 */
public final class StubbedHashingAlgorithm<A> implements HashingAlgorithm<A> {
    private static final StubbedHashingAlgorithm<?> INSTANCE = new StubbedHashingAlgorithm<>(empty());

    private final HashMap<A, Integer> table;

    private StubbedHashingAlgorithm(HashMap<A, Integer> table) {
        this.table = table;
    }

    @Override
    public Integer checkedApply(A a) {
        return table.get(a).orElseGet(() -> Objects.hashCode(a));
    }

    /**
     * Associate the value <code>a</code> with the given <code>hash</code> such that any subsequent invocation
     * of {@link HashingAlgorithm#apply(Object) apply(a)} will return <code>hash</code> (unless overridden).
     *
     * @param a    the value to stub the hash for
     * @param hash the stubbed hash
     * @return the updated {@link StubbedHashingAlgorithm}
     */
    public StubbedHashingAlgorithm<A> stub(A a, Integer hash) {
        return new StubbedHashingAlgorithm<>(table.put(a, hash));
    }

    /**
     * The singleton empty {@link StubbedHashingAlgorithm}.
     *
     * @param <A> the type to hash
     * @return the singleton empty {@link StubbedHashingAlgorithm}
     */
    @SuppressWarnings("unchecked")
    public static <A> StubbedHashingAlgorithm<A> stubbedHashingAlgorithm() {
        return (StubbedHashingAlgorithm<A>) INSTANCE;
    }
}
