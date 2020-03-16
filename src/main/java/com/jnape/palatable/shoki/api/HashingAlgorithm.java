package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.functions.Fn1;

import java.util.Objects;

/**
 * A {@link HashingAlgorithm hashing algorithm} is an arrow <code>A -&gt; Integer</code>. Good
 * hashing algorithms are generally fast and uniformly distribute values of type <code>A</code> across values of type
 * {@link Integer} (modulo cardinality differences where <code>A</code>'s cardinality exceeds {@link Integer}'s).
 *
 * @param <A> the type to hash
 */
public interface HashingAlgorithm<A> extends Fn1<A, Integer> {

    /**
     * A {@link HashingAlgorithm} implemented in terms of {@link Objects#hashCode(Object)}.
     *
     * @param <A> the type to hash
     * @return a {@link HashingAlgorithm} implemented in terms of {@link Objects#hashCode(Object)}.
     */
    static <A> HashingAlgorithm<A> objectHashCode() {
        return Objects::hashCode;
    }

    /**
     * A {@link HashingAlgorithm} implemented in terms of {@link System#identityHashCode(Object)}.
     *
     * @param <A> the type to hash
     * @return a {@link HashingAlgorithm} implemented in terms of {@link System#identityHashCode(Object)}.
     */
    static <A> HashingAlgorithm<A> identityHashCode() {
        return System::identityHashCode;
    }
}
