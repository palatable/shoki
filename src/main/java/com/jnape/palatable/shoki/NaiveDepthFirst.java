package com.jnape.palatable.shoki;

import java.util.Iterator;

public final class NaiveDepthFirst {

    private NaiveDepthFirst() {
    }

    public static final class InOrder<A> implements DepthFirst<A> {
        @Override
        public Iterator<A> iterator() {
            throw new UnsupportedOperationException();
        }
    }

    public static final class PostOrder<A> implements DepthFirst<A> {
        @Override
        public Iterator<A> iterator() {
            throw new UnsupportedOperationException();
        }
    }
}
