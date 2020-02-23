package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.functions.Fn1;

import java.util.Objects;

public interface HashingAlgorithm<A> extends Fn1<A, Integer> {

    static <A> HashingAlgorithm<A> objectHashCode() {
        return Objects::hash;
    }
}
