package com.jnape.palatable.shoki.api.alpha3;

import com.jnape.palatable.lambda.adt.choice.Choice2;
import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn0;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.shoki.api.Sizable;
import com.jnape.palatable.shoki.api.alpha3.SizeInfo.Sized.Finite.Computed;
import com.jnape.palatable.shoki.api.alpha3.SizeInfo.Sized.Finite.Computed.EveryTime;
import com.jnape.palatable.shoki.api.alpha3.SizeInfo.Sized.Finite.Computed.Once;
import com.jnape.palatable.shoki.api.alpha3.SizeInfo.Sized.Finite.Known;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.choice.Choice2.a;
import static com.jnape.palatable.lambda.adt.choice.Choice2.b;
import static com.jnape.palatable.shoki.api.alpha3.SizeInfo.Sized.Infinite;

/**
 * Algebraic data type representing the possible constructions of a {@link SizeInfo}.
 *
 * @see Sizable
 */
public abstract class SizeInfo implements CoProduct2<SizeInfo.Sized, SizeInfo.Unsized, SizeInfo> {

    private SizeInfo() {
    }

    /**
     * The singleton {@link Unsized unsized} {@link SizeInfo}.
     *
     * @return the {@link Unsized unsized} {@link SizeInfo}
     */
    public static Unsized unsized() {
        return Unsized.INSTANCE;
    }

    /**
     * The singleton {@link Infinite infinite} {@link SizeInfo}.
     *
     * @return the {@link Infinite infinite} {@link SizeInfo}
     */
    public static Infinite infinite() {
        return Infinite.INSTANCE;
    }

    /**
     * Construct a {@link SizeInfo} with a {@link Known known} numeric size of type <code>Size</code>.
     *
     * @param size   the known size
     * @param <Size> the size type
     * @return the {@link Known known} {@link SizeInfo}
     */
    public static <Size extends Number> Known<Size> known(Size size) {
        return new Known<>(size);
    }

    /**
     * Construct a {@link SizeInfo} that {@link Computed computes} a numeric size of type <code>Size</code>
     * {@link Once once} and memoizes the result thereafter.
     *
     * @param thunk  the computation
     * @param <Size> the size type
     * @return the {@link Once once-computed} {@link SizeInfo}
     */
    public static <Size extends Number> Once<Size> computedOnce(Fn0<Size> thunk) {
        return new Once<>(thunk);
    }

    /**
     * Construct a {@link SizeInfo} that {@link Computed computes} a numeric size of type <code>Size</code>
     * {@link EveryTime every time}.
     *
     * @param thunk  the computation
     * @param <Size> the size type
     * @return the {@link EveryTime always-computed} {@link SizeInfo}
     */
    public static <Size extends Number> EveryTime<Size> computedEveryTime(Fn0<Size> thunk) {
        return new EveryTime<>(thunk);
    }

    /**
     * A {@link SizeInfo} representing some knowledge about the representative size of some value.
     */
    public static abstract class Sized extends SizeInfo {

        private Sized() {
        }

        @Override
        public final <R> R match(Fn1<? super Sized, ? extends R> aFn,
                                 Fn1<? super Unsized, ? extends R> bFn) {
            return aFn.apply(this);
        }

        /**
         * {@link Choice2 Choose} between the constructions of this {@link Sized} {@link SizeInfo} based on whether
         * the cardinality is {@link Finite} or {@link Infinite}.
         *
         * @return {@link Choice2 whether or not} this {@link Sized Sized} is {@link Finite finite} or
         * {@link Infinite infinite}
         */
        public abstract Choice2<Finite<?>, Infinite> cardinality();

        /**
         * A {@link SizeInfo} representing a finite size
         *
         * @param <Size> the numeric size type
         */
        public static abstract class Finite<Size extends Number> extends Sized {
            private Finite() {
            }

            /**
             * {@inheritDoc}
             */
            @Override
            public final Choice2<Finite<?>, Infinite> cardinality() {
                return a(this);
            }

            /**
             * {@link Choice2 Choose} between the constructions of this {@link Finite} {@link SizeInfo} based on whether
             * the size is {@link Known} or {@link Computed}.
             *
             * @return {@link Choice2 whether or not} this {@link Sized Sized} is {@link Known known} or
             * {@link Computed computed}
             */
            public abstract Choice2<Known<Size>, Computed<Size>> availability();

            /**
             * A {@link SizeInfo} representing a known size of some value.
             *
             * @param <Size> the numeric size type
             */
            public static final class Known<Size extends Number> extends Finite<Size> {
                private final Size size;

                private Known(Size size) {
                    this.size = size;
                }

                /**
                 * Retrieve the known numeric size value.
                 *
                 * @return the known size
                 */
                public Size getSize() {
                    return size;
                }

                /**
                 * {@inheritDoc}
                 */
                @Override
                public Choice2<Known<Size>, Computed<Size>> availability() {
                    return a(this);
                }

                @Override
                public boolean equals(Object other) {
                    return other instanceof Known<?> && Objects.equals(size, ((Known<?>) other).size);
                }

                @Override
                public int hashCode() {
                    return Objects.hashCode(size);
                }

                @Override
                public String toString() {
                    return "SizeInfo.Finite.Known[" + size + "]";
                }
            }

            /**
             * A {@link SizeInfo} representing a size of some value to be computed.
             *
             * @param <Size> the numeric size type
             */
            public static abstract class Computed<Size extends Number> extends Finite<Size> {

                private Computed() {
                }

                /**
                 * {@inheritDoc}
                 */
                @Override
                public final Choice2<Known<Size>, Computed<Size>> availability() {
                    return Choice2.b(this);
                }

                /**
                 * {@link Choice2 Choose} between the constructions of this {@link Computed computed}
                 * {@link SizeInfo SizeInfo} based on whether the evaluation happens {@link Once once} or
                 * {@link EveryTime every time}.
                 *
                 * @return {@link Choice2 whether or not} this {@link Computed Computed} is evaluates {@link Once once}
                 * or {@link EveryTime every time}
                 */
                public abstract Choice2<Once<Size>, EveryTime<Size>> evaluation();

                /**
                 * A {@link SizeInfo SizeInfo} that is {@link Computed computed} once and is memoized thereafter.
                 *
                 * @param <Size> the numeric size type
                 */
                public static final class Once<Size extends Number> extends Computed<Size> {

                    private volatile Fn0<Size> thunk;
                    private volatile Size      size;

                    private Once(Fn0<Size> thunk) {
                        this.thunk = thunk;
                    }

                    /**
                     * Compute and memoize the size, or return the memoized size if already computed.
                     *
                     * @return the computed or memoized size
                     */
                    public Size getOrCompute() {
                        Size size = this.size;
                        if (size == null) {
                            synchronized (this) {
                                size = this.size;
                                if (size == null) {
                                    Fn0<Size> thunk = this.thunk;
                                    this.size  = size = thunk.apply();
                                    this.thunk = null;
                                }
                            }
                        }
                        return size;
                    }

                    /**
                     * {@inheritDoc}
                     */
                    @Override
                    public Choice2<Once<Size>, EveryTime<Size>> evaluation() {
                        return Choice2.a(this);
                    }

                    @Override
                    public String toString() {
                        return "SizeInfo.Finite.Computed.Once[" + (size == null ? "…" : size) + "]";
                    }
                }

                /**
                 * A {@link SizeInfo SizeInfo} that is {@link Computed computed} every time it is demanded.
                 *
                 * @param <Size> the numeric size type
                 */
                public static final class EveryTime<Size extends Number> extends Computed<Size> {

                    private final Fn0<Size> thunk;

                    private EveryTime(Fn0<Size> thunk) {
                        this.thunk = thunk;
                    }

                    /**
                     * Compute the size.
                     *
                     * @return the computed size
                     */
                    public Size compute() {
                        return thunk.apply();
                    }

                    /**
                     * {@inheritDoc}
                     */
                    @Override
                    public Choice2<Once<Size>, EveryTime<Size>> evaluation() {
                        return Choice2.b(this);
                    }

                    @Override
                    public String toString() {
                        return "SizeInfo.Finite.Computed.Always[…]";
                    }
                }
            }
        }

        /**
         * A {@link SizeInfo} representing an infinite size
         */
        public static final class Infinite extends Sized {
            private static final Infinite INSTANCE = new Infinite();

            private Infinite() {
            }

            /**
             * {@inheritDoc}
             */
            @Override
            public Choice2<Finite<?>, Infinite> cardinality() {
                return b(this);
            }

            @Override
            public String toString() {
                return "SizeInfo.Sized.Infinite";
            }
        }
    }

    /**
     * A {@link SizeInfo} that advertises no assumptions about the representative size of some value.
     */
    public static final class Unsized extends SizeInfo {
        private static final Unsized INSTANCE = new Unsized();

        private Unsized() {
        }

        @Override
        public <R> R match(Fn1<? super Sized, ? extends R> aFn,
                           Fn1<? super Unsized, ? extends R> bFn) {
            return bFn.apply(this);
        }

        @Override
        public String toString() {
            return "SizeInfo.Unsized";
        }
    }
}
