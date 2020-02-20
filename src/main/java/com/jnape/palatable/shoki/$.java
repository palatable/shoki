package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.functions.Fn0;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.lambda.functor.Functor;


public interface $<A> extends Functor<A, $<?>> {

    A force();

    @Override
    default <B> $<B> fmap(Fn1<? super A, ? extends B> fn) {
        return $(() -> fn.apply(force()));
    }

    default $<A> memoize() {
        return new Memoized<>(this::force);
    }

    static <A> $<A> $(Fn0<A> fn0) {
        return fn0::apply;
    }

    static <A> $<A> memoize(Fn0<A> Fn0) {
        return new Memoized<>(Fn0);
    }

    final class Memoized<A> implements $<A> {
        private final    Fn0<A>  fn0;
        private volatile A       result;
        private volatile boolean computed;

        private Memoized(Fn0<A> Fn0) {
            this.fn0 = Fn0;
            computed = false;
        }

        @Override
        public <B> Memoized<B> fmap(Fn1<? super A, ? extends B> fn) {
            return new Memoized<>(() -> fn.apply(force()));
        }

        @Override
        public A force() {
            if (!computed) {
                synchronized (this) {
                    if (!computed) {
                        result = fn0.apply();
                        computed = true;
                    }
                }
            }
            return result;
        }
    }
}
