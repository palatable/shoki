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
        return new Suspended<>(fn0);
    }

    static <A> $<A> $(A a) {
        return new Value<>(a);
    }

    static <A> $<A> memoize(Fn0<A> Fn0) {
        return new Memoized<>(Fn0);
    }

    final class Suspended<A> implements $<A> {
        private final Fn0<A> fn0;

        private Suspended(Fn0<A> fn0) {
            this.fn0 = fn0;
        }

        @Override
        public A force() {
            return fn0.apply();
        }

        @Override
        public String toString() {
            return "Suspended{" +
                "fn0=" + fn0 +
                '}';
        }
    }

    final class Memoized<A> implements $<A> {
        private final    Fn0<A>  fn0;
        private volatile A       result;
        private volatile boolean computed;

        private Memoized(Fn0<A> fn0) {
            this.fn0 = fn0;
            computed = false;
        }

        @Override
        public <B> Memoized<B> fmap(Fn1<? super A, ? extends B> fn) {
            return new Memoized<>(() -> fn.apply(force()));
        }

        @Override
        public Memoized<A> memoize() {
            return this;
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

        @Override
        public String toString() {
            return "Memoized{" + (computed ? result : "Not computed") + "}";
        }
    }

    final class Value<A> implements $<A> {
        private final A a;

        private Value(A a) {
            this.a = a;
        }

        @Override
        public A force() {
            return a;
        }

        @Override
        public <B> Value<B> fmap(Fn1<? super A, ? extends B> fn) {
            return new Value<>(fn.apply(a));
        }

        @Override
        public Value<A> memoize() {
            return this;
        }

        @Override
        public String toString() {
            return "Value{a=" + a + "}";
        }
    }
}
