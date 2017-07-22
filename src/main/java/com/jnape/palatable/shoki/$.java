package com.jnape.palatable.shoki;

import java.util.function.Supplier;

public interface $<A> extends Supplier<A> {

    @Override
    default A get() {
        return force();
    }

    A force();

    default $<A> memoize() {
        return new Memoized<>(this);
    }

    static <A> $<A> $(Supplier<A> supplier) {
        return supplier::get;
    }

    static <A> $<A> memoize(Supplier<A> supplier) {
        return new Memoized<>(supplier);
    }

    final class Memoized<A> implements $<A> {
        private final    Supplier<A> supplier;
        private volatile A           result;
        private volatile boolean     computed;

        private Memoized(Supplier<A> supplier) {
            this.supplier = supplier;
            computed = false;
        }

        @Override
        public A force() {
            if (!computed) {
                synchronized (this) {
                    if (!computed) {
                        result = supplier.get();
                        computed = true;
                    }
                }
            }
            return result;
        }
    }
}
