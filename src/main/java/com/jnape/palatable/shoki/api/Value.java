package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.choice.Choice2;
import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn0;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.shoki.api.Value.ComputedAtMostOnce.Known;
import com.jnape.palatable.shoki.api.Value.ComputedAtMostOnce.Memoized;

import java.util.Objects;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;

public abstract class Value<A> implements
        CoProduct2<Value.ComputedAtMostOnce<A>, Value.ComputedEveryTime<A>, Value<A>> {

    private Value() {
    }

    /**
     * The underlying value.
     *
     * @return the underlying value
     */
    public abstract A getOrCompute();

    @Override
    public final boolean equals(Object other) {
        return other instanceof Value<?> && Objects.equals(getOrCompute(), ((Value<?>) other).getOrCompute());
    }

    @Override
    public final int hashCode() {
        return getOrCompute().hashCode();
    }

    public static <A> Known<A> known(A a) {
        return new Known<>(a);
    }

    /**
     * Construct a {@link Value} that computes a value of type <code>A</code> and {@link Memoized memoizes} the result
     * thereafter.
     *
     * @param memo  the memo to use
     * @param thunk the computation
     * @param <A>   the computed value type
     * @return the {@link Memoized memoized} {@link Value}
     */
    public static <A> Memoized<A> memoized(Memo<A> memo, Fn0<A> thunk) {
        return new Memoized<>(memo, thunk);
    }

    /**
     * Construct a {@link Value} that computes a value of type <code>A</code> {@link ComputedEveryTime every time}.
     *
     * @param thunk the computation
     * @param <A>   the computed value type
     * @return the {@link ComputedEveryTime always-computed} {@link Value}
     */
    public static <A> ComputedEveryTime<A> computedEveryTime(Fn0<A> thunk) {
        return new ComputedEveryTime<>(thunk);
    }

    public static abstract class ComputedAtMostOnce<A> extends Value<A> {

        private ComputedAtMostOnce() {
        }

        /**
         * {@link Choice2 Choose} between the constructions of this
         * {@link ComputedAtMostOnce potentially computed value} based on whether the value is {@link Known known} or
         * {@link Memoized memoized}.
         *
         * @return {@link Choice2 whether or not} this {@link Value} is {@link Known known} or
         * {@link Memoized memoized}.
         */
        public abstract Choice2<Known<A>, Memoized<A>> evaluation();

        /**
         * {@inheritDoc}
         */
        @Override
        public final <R> R match(Fn1<? super ComputedAtMostOnce<A>, ? extends R> aFn,
                                 Fn1<? super ComputedEveryTime<A>, ? extends R> bFn) {
            return aFn.apply(this);
        }

        /**
         * A {@link Value} representing a known value.
         *
         * @param <A> the known value type
         */
        public static final class Known<A> extends ComputedAtMostOnce<A> {
            private final A a;

            private Known(A a) {
                this.a = a;
            }

            /**
             * Retrieve the known value.
             *
             * @return the known value
             */
            public A get() {
                return a;
            }

            /**
             * {@inheritDoc}
             *
             * @see Known#get()
             */
            @Override
            public A getOrCompute() {
                return get();
            }

            /**
             * {@inheritDoc}
             */
            @Override
            public Choice2<Known<A>, Memoized<A>> evaluation() {
                return Choice2.a(this);
            }

            @Override
            public String toString() {
                return "Value.ComputedAtMostOnce.Known[" + a + "]";
            }
        }

        /**
         * A {@link Value} that is {@link ComputedEveryTime computed} once and is memoized thereafter.
         *
         * @param <A> the computed value type
         */
        public static final class Memoized<A> extends ComputedAtMostOnce<A> {

            private final Memo<A> memo;
            private final Fn0<A>  thunk;

            private Memoized(Memo<A> memo, Fn0<A> thunk) {
                this.thunk = thunk;
                this.memo  = memo;
            }

            /**
             * {@inheritDoc}
             * <p>
             * If this value is not already computed, compute and memoize it; otherwise, return the memoized value.
             */
            @Override
            public A getOrCompute() {
                return memo.getOrCompute(thunk);
            }

            /**
             * {@inheritDoc}
             */
            @Override
            public Choice2<Known<A>, Memoized<A>> evaluation() {
                return Choice2.b(this);
            }


            @Override
            public String toString() {
                return "Value.ComputedAtMostOnce.Memoized["
                        + (memo.get().match(constantly("…"), Object::toString))
                        + "]";
            }
        }
    }

    /**
     * A {@link Value} that is {@link ComputedEveryTime computed} every time it is demanded.
     *
     * @param <A> the computed value type
     */
    public static final class ComputedEveryTime<A> extends Value<A> {
        private final Fn0<A> thunk;

        private ComputedEveryTime(Fn0<A> thunk) {
            this.thunk = thunk;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public A getOrCompute() {
            return thunk.apply();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public <R> R match(Fn1<? super ComputedAtMostOnce<A>, ? extends R> aFn,
                           Fn1<? super ComputedEveryTime<A>, ? extends R> bFn) {
            return bFn.apply(this);
        }

        @Override
        public String toString() {
            return "Value.ComputedEveryTime[…]";
        }
    }
}
