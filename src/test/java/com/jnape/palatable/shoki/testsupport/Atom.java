package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.shoki.api.Memo;

import java.util.concurrent.atomic.AtomicReference;

import static com.jnape.palatable.lambda.adt.Maybe.maybe;

public final class Atom<A> implements Memo<A> {
    private final AtomicReference<A> ref;

    private Atom(AtomicReference<A> ref) {
        this.ref = ref;
    }

    @Override
    public A getOrElse(A ifMissing) {
        return maybe(ref.get()).orElse(ifMissing);
    }

    @Override
    public void set(A a) {
        ref.set(a);
    }

    @Override
    public Object lock() {
        return ref;
    }

    public static <A> Atom<A> atom() {
        return new Atom<>(new AtomicReference<>());
    }
}
