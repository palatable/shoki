package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.functions.Fn0;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import static com.jnape.palatable.lambda.adt.Maybe.maybe;

public interface Memo<A> {

    Maybe<A> get();

    default A getOrCompute(Fn0<? extends A> thunk) {
        return get().orElseGet(thunk::apply);
    }

    static <Instance, A> Memo<A> volatileField(Instance instance, AtomicReferenceFieldUpdater<Instance, A> updater) {
        final class VolatileField implements Memo<A> {
            @Override
            public Maybe<A> get() {
                return maybe(updater.get(instance));
            }

            @Override
            public A getOrCompute(Fn0<? extends A> thunk) {
                A a = updater.get(instance);
                if (Objects.equals(null, a)) {
                    synchronized (instance) {
                        a = updater.get(instance);
                        if (Objects.equals(null, a)) {
                            updater.set(instance, a = thunk.apply());
                        }
                    }
                }
                return a;
            }
        }

        return new VolatileField();
    }
}
