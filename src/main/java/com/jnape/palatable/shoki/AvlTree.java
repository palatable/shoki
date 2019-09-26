package com.jnape.palatable.shoki;

public interface AvlTree<A extends Comparable<A>, AvlTree extends com.jnape.palatable.shoki.AvlTree<A, AvlTree>> extends BinarySearchTree<A, AvlTree> {

    AvlTree left();

    AvlTree right();
}
