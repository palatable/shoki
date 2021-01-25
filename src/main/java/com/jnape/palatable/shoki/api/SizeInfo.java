package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.choice.Choice2;
import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.choice.Choice2.a;
import static com.jnape.palatable.lambda.adt.choice.Choice2.b;
import static com.jnape.palatable.shoki.api.SizeInfo.Sized.Infinite;

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
     * Construct a {@link Finite finite} {@link SizeInfo} of the given <code>Size</code>.
     *
     * @param size   the size
     * @param <Size> the size type
     * @return the {@link Finite} {@link SizeInfo}
     */
    public static <Size extends Number> Finite<Size> finite(Size size) {
        return new Finite<>(size);
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
        public static final class Finite<Size extends Number> extends Sized {

            private final Size size;

            private Finite(Size size) {
                this.size = size;
            }

            /**
             * The underlying size.
             *
             * @return the underlying size
             */
            public Size size() {
                return size;
            }

            /**
             * {@inheritDoc}
             */
            @Override
            public final Choice2<Finite<?>, Infinite> cardinality() {
                return a(this);
            }

            @Override
            public final boolean equals(Object other) {
                return other instanceof Finite<?> && Objects.equals(size, ((Finite<?>) other).size);
            }

            @Override
            public final int hashCode() {
                return size.hashCode();
            }

            @Override
            public String toString() {
                return "SizeInfo.Sized.Finite[" + size + ']';
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
