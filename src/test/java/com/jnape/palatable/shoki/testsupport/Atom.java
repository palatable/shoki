package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.functions.Fn0;
import com.jnape.palatable.shoki.api.Memo;

import java.util.concurrent.atomic.AtomicReference;

import static com.jnape.palatable.lambda.adt.Maybe.maybe;

public final class Atom<A> implements Memo<A> {
    private final AtomicReference<A> ref;

    private Atom(AtomicReference<A> ref) {
        this.ref = ref;
    }

    @Override
    public Maybe<A> get() {
        return maybe(ref.get());
    }

    @Override
    public synchronized A getOrCompute(Fn0<? extends A> thunk) {
        A a = ref.get();
        if (a == null)
            ref.set(a = thunk.apply());
        return a;
    }

    public static <A> Atom<A> atom() {
        return new Atom<>(new AtomicReference<>());
    }
}
