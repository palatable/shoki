package com.jnape.palatable.shoki;

public interface Tree<A, T extends Tree<A, T>> {

    Iterable<T> branches();
}
