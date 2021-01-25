package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.choice.Choice2;
import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn0;
import com.jnape.palatable.lambda.functions.Fn1;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.choice.Choice2.a;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;

public abstract class Value<A> implements CoProduct2<Value.Known<A>, Value.Computed<A>, Value<A>> {

    private Value() {
    }

    /**
     * {@link Known Get} or {@link Computed compute} the underlying value.
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
     * Construct a {@link Value} that {@link Computed computes} a value of type <code>A</code>
     * {@link Computed.Once once} and memoizes the result thereafter.
     *
     * @param memo  the memo to use
     * @param thunk the computation
     * @param <A>   the computed value type
     * @return the {@link Computed.Once once-computed} {@link Value}
     */
    public static <A> Computed.Once<A> computedOnce(Memo<A> memo, Fn0<A> thunk) {
        return new Computed.Once<>(memo, thunk);
    }

    /**
     * Construct a {@link Value} that {@link Computed computes} a value of type <code>A</code>
     * {@link Computed.EveryTime every time}.
     *
     * @param thunk the computation
     * @param <A>   the computed value type
     * @return the {@link Computed.EveryTime always-computed} {@link Value}
     */
    public static <A> Computed.EveryTime<A> computedEveryTime(Fn0<A> thunk) {
        return new Computed.EveryTime<>(thunk);
    }

    /**
     * A {@link Value} representing a known value.
     *
     * @param <A> the known value type
     */
    public static final class Known<A> extends Value<A> {
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

        @Override
        public <R> R match(Fn1<? super Known<A>, ? extends R> aFn,
                           Fn1<? super Computed<A>, ? extends R> bFn) {
            return aFn.apply(this);
        }

        @Override
        public String toString() {
            return "Value.Known[" + a + "]";
        }
    }

    /**
     * A {@link Value} representing a value to be computed.
     *
     * @param <A> the computed value type
     */
    public static abstract class Computed<A> extends Value<A> {

        private Computed() {
        }

        @Override
        public final <R> R match(Fn1<? super Known<A>, ? extends R> aFn,
                                 Fn1<? super Computed<A>, ? extends R> bFn) {
            return bFn.apply(this);
        }

        /**
         * {@link Choice2 Choose} between the constructions of this {@link Computed computed value} based on whether
         * the evaluation happens {@link Computed.Once once} or {@link Computed.EveryTime every time}.
         *
         * @return {@link Choice2 whether or not} this {@link Computed Computed} is evaluated {@link Once once} or
         * {@link EveryTime every time}
         */
        public abstract Choice2<Computed.Once<A>, EveryTime<A>> evaluation();

        /**
         * A {@link Value} that is {@link Computed computed} once and is memoized thereafter.
         *
         * @param <A> the computed value type
         */
        public static final class Once<A> extends Computed<A> {

            private final Memo<A> memo;
            private final Fn0<A>  thunk;

            private Once(Memo<A> memo, Fn0<A> thunk) {
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
             * Returns <code>true</code> if this {@link Once} has already been computed; <code>false</code> otherwise.
             *
             * @return whether or not this {@link Once} has already been computed
             */
            public boolean isComputed() {
                return memo.get().match(constantly(false), constantly(true));
            }

            /**
             * {@inheritDoc}
             */
            @Override
            public Choice2<Once<A>, EveryTime<A>> evaluation() {
                return a(this);
            }

            @Override
            public String toString() {
                return "Value.Computed.Once[" + (memo.get().match(constantly("…"), Object::toString)) + "]";
            }
        }

        /**
         * A {@link Value} that is {@link Computed computed} every time it is demanded.
         *
         * @param <A> the computed value type
         */
        public static final class EveryTime<A> extends Computed<A> {

            private final Fn0<A> thunk;

            private EveryTime(Fn0<A> thunk) {
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
            public Choice2<Once<A>, EveryTime<A>> evaluation() {
                return Choice2.b(this);
            }

            @Override
            public String toString() {
                return "Value.Computed.EveryTime[…]";
            }
        }
    }
}
