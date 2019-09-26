package com.jnape.palatable.shoki;

public interface BinarySearchTree<A extends Comparable<A>, BST extends BinarySearchTree<A, BST>> extends BinaryTree<A, BST> {

    @Override
    BST left();

    @Override
    BST right();

    boolean contains(A a);

    @Override
    Iterable<BST> branches();
}
