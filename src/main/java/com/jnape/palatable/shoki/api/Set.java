package com.jnape.palatable.shoki.api;

public interface Set<Size extends Number, A> extends Collection<Size, A>, Membership<A> {

    Set<Size, A> add(A a);

    Set<Size, A> remove(A a);

    @Override
    Set<Size, A> tail();
}
