package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.Try;
import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn1;

import java.math.BigInteger;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.Try.success;
import static com.jnape.palatable.lambda.adt.Try.trying;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Id.id;
import static com.jnape.palatable.lambda.functions.builtin.fn2.GT.gt;
import static com.jnape.palatable.lambda.functions.builtin.fn2.LT.lt;
import static java.lang.Math.addExact;
import static java.lang.Math.min;
import static java.math.BigInteger.ZERO;

/**
 * A {@link Number} type representing the
 * <a href="https://en.wikipedia.org/wiki/Natural_number" target="_new">natural</a> numbers - the set of non-negative
 * integers - with type-safe interfaces for dealing with the {@link CoProduct2 coproduct} of {@link Zero zero} and
 * {@link NonZero non-zero} values.
 */
public abstract class Natural extends Number
        implements CoProduct2<Natural.Zero, Natural.NonZero, Natural>, Comparable<Natural> {

    private Natural() {
    }

    /**
     * The non-truncated underlying integral value as a {@link java.math.BigInteger}.
     *
     * @return the value
     */
    public abstract BigInteger bigIntegerValue();

    /**
     * Addition of two {@link Natural} numbers.
     *
     * @param addend the {@link Natural} addend
     * @return the {@link Natural} sum
     */
    public abstract Natural plus(Natural addend);

    /**
     * Specialized subtraction when the subtrahend is {@link Zero}, guaranteeing a {@link Natural} difference
     * (the minuend).
     *
     * @param subtrahend the {@link Zero} subtrahend
     * @return the {@link Natural} minuend
     */
    public abstract Natural minus(Zero subtrahend);

    /**
     * Subtraction of two {@link Natural} numbers. If the difference is {@link Natural}, {@link Maybe#just(Object) just}
     * return it; otherwise, the difference would be negative, so return {@link Maybe#nothing()}.
     *
     * @param subtrahend the {@link Natural} subtrahend
     * @return {@link Maybe} the {@link Natural} difference
     */
    public final Maybe<Natural> minus(Natural subtrahend) {
        return subtrahend.match(constantly(just(this)), this::minus);
    }

    /**
     * Specialized addition when the addend is {@link NonZero}, guaranteeing a {@link NonZero} sum.
     *
     * @param addend the {@link Natural} addend
     * @return the {@link NonZero} sum
     */
    public final NonZero plus(NonZero addend) {
        return match(constantly(addend), augend -> augend.plus((Natural) addend));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract int intValue();

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract long longValue();

    /**
     * {@inheritDoc}
     */
    @Override
    public final float floatValue() {
        return intValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final double doubleValue() {
        return longValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Natural other) {
        return bigIntegerValue().compareTo(other.bigIntegerValue());
    }

    protected abstract Maybe<Natural> minus(NonZero subtrahend);

    /**
     * The singleton {@link Zero zero} value, the smallest {@link Natural}.
     *
     * @return the singleton {@link Zero}
     */
    public static Zero zero() {
        return Zero.INSTANCE;
    }

    /**
     * A convenience method for offering a {@link NonZero non-zero} value of <code>1</code>.
     *
     * @return the {@link NonZero non-zero} value of 1
     */
    public static NonZero one() {
        return NonZero.I.ONE;
    }

    /**
     * If <code>value</code> is non-negative, return {@link Maybe#just(Object) just} the corresponding {@link Natural};
     * otherwise, return {@link Maybe#nothing() nothing}.
     *
     * @param value the value
     * @return {@link Maybe} the corresponding {@link Natural}
     */
    public static Maybe<Natural> natural(int value) {
        return natural(value, 0);
    }

    /**
     * Convenience overload of {@link Natural#natural(int)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return {@link Maybe} the corresponding {@link Natural}
     * @see Natural#natural(int)
     */
    public static Maybe<Natural> natural(long value) {
        return natural(value, 0L);
    }

    /**
     * Convenience overload of {@link Natural#natural(int)} allowing {@link BigInteger} values.
     *
     * @param value the value
     * @return {@link Maybe} the corresponding {@link Natural}
     * @see Natural#natural(int)
     */
    public static Maybe<Natural> natural(BigInteger value) {
        return natural(value, ZERO);
    }

    /**
     * Return the corresponding {@link Natural} for the
     * <a href="https://en.wikipedia.org/wiki/Absolute_value" target="_new">absolute value</a> of the given
     * <code>value</code>.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the value's absolute value
     * @see Natural#abs(long)
     */
    public static Natural abs(BigInteger value) {
        return abs(value, ZERO, BigInteger::negate);
    }

    /**
     * Convenience overload of {@link Natural#abs(BigInteger)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the value's absolute value
     * @see Natural#abs(BigInteger)
     */
    public static Natural abs(long value) {
        return abs(value, 0L, l -> -l);
    }

    /**
     * Convenience overload of {@link Natural#abs(BigInteger)} allowing <code>int</code> values.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the value's absolute value
     * @see Natural#abs(BigInteger)
     */
    public static Natural abs(int value) {
        return abs(value, 0, i -> -i);
    }

    /**
     * Return the corresponding {@link Natural} for the given <code>value</code>, defaulting to {@link Zero zero} if
     * <code>value</code> is negative.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the given value, or {@link Zero}
     * @see Natural#clampZero(long)
     */
    public static Natural clampZero(BigInteger value) {
        return clampZero(value, ZERO);
    }

    /**
     * Convenience overload of {@link Natural#clampZero(BigInteger)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the given value, or {@link Zero}
     * @see Natural#clampZero(BigInteger)
     */
    public static Natural clampZero(long value) {
        return clampZero(value, 0L);
    }

    /**
     * Convenience overload of {@link Natural#clampZero(BigInteger)} allowing <code>int</code> values.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the given value, or {@link Zero}
     * @see Natural#clampZero(BigInteger)
     */
    public static Natural clampZero(int value) {
        return clampZero(value, 0);
    }

    /**
     * Return the corresponding {@link NonZero non-zero} {@link Natural} for the given <code>value</code>, defaulting to
     * {@link Natural#one() one} if <code>value</code> is {@link BigInteger#ZERO zero} or negative.
     *
     * @param value the value
     * @return the {@link NonZero non-zero} {@link Natural} corresponding to the given value, or {@link Natural#one()}
     * @see Natural#clampOne(long)
     */
    public static NonZero clampOne(BigInteger value) {
        return clampOne(value, ZERO);
    }

    /**
     * Convenience overload of {@link Natural#clampOne(BigInteger)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return the {@link NonZero non-zero} {@link Natural} corresponding to the given value, or {@link Natural#one()}
     * {@link Natural#one() one}
     * @see Natural#clampOne(BigInteger)
     */
    public static NonZero clampOne(long value) {
        return clampOne(value, 0L);
    }

    /**
     * Convenience overload of {@link Natural#clampOne(BigInteger)} allowing <code>int</code> values.
     *
     * @param value the value
     * @return the {@link NonZero non-zero} {@link Natural} corresponding to the given value, or {@link Natural#one()}
     * {@link Natural#one() one}
     * @see Natural#clampOne(BigInteger)
     */
    public static NonZero clampOne(int value) {
        return clampOne(value, 0);
    }

    private static <N extends Number & Comparable<N>> Maybe<Natural> natural(N value, N zero) {
        return gt(zero, value) ? just(NonZero.nonZero(value)) : lt(zero, value) ? nothing() : just(zero());
    }

    private static <N extends Number & Comparable<N>> Natural abs(N value, N zero, Fn1<? super N, ? extends N> negate) {
        return natural(value, zero).orElseGet(() -> NonZero.nonZero(negate.apply(value)));
    }

    private static <N extends Number & Comparable<N>> Natural clampZero(N value, N zero) {
        return natural(value, zero).orElse(zero());
    }

    private static <N extends Number & Comparable<N>> NonZero clampOne(N value, N zero) {
        return clampZero(value, zero).match(constantly(one()), id());
    }

    /**
     * The type corresponding to the {@link Natural} zero term.
     *
     * @see Natural
     * @see NonZero
     */
    public static final class Zero extends Natural {
        private static final Zero INSTANCE = new Zero();

        private Zero() {
        }

        /**
         * Specialized addition when both the augend and addend are {@link Zero}, guaranteeing a {@link Zero} sum.
         *
         * @param addend the {@link Zero} addend
         * @return {@link Zero}
         */
        public Zero plus(@SuppressWarnings("unused") Zero addend) {
            return this;
        }

        @Override
        public Natural plus(Natural addend) {
            return addend;
        }

        @Override
        public Zero minus(Zero subtrahend) {
            return this;
        }

        @Override
        public Maybe<Natural> minus(NonZero subtrahend) {
            return nothing();
        }

        @Override
        public BigInteger bigIntegerValue() {
            return ZERO;
        }

        @Override
        public int intValue() {
            return 0;
        }

        @Override
        public long longValue() {
            return 0L;
        }

        @Override
        public <R> R match(Fn1<? super Zero, ? extends R> aFn, Fn1<? super NonZero, ? extends R> bFn) {
            return aFn.apply(this);
        }

        @Override
        public int hashCode() {
            return 0;
        }

        @Override
        public String toString() {
            return "Zero{}";
        }
    }

    /**
     * The type corresponding to all non-zero {@link Natural} terms.
     *
     * @see Natural
     * @see NonZero
     */
    public static abstract class NonZero extends Natural {

        private NonZero() {
        }

        abstract Number value();

        @Override
        public abstract NonZero plus(Natural addend);

        @Override
        public NonZero minus(Zero subtrahend) {
            return this;
        }

        @Override
        public <R> R match(Fn1<? super Zero, ? extends R> aFn, Fn1<? super NonZero, ? extends R> bFn) {
            return bFn.apply(this);
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof NonZero && Objects.equals(bigIntegerValue(), ((NonZero) other).bigIntegerValue());
        }

        @Override
        public int hashCode() {
            return Objects.hashCode(value());
        }

        @Override
        public String toString() {
            return "NonZero{value=" + value() + '}';
        }

        private static NonZero nonZero(Number n) {
            if (n instanceof Integer) {
                return new I((int) n);
            } else if (n instanceof Long) {
                long nLong = (Long) n;
                return nLong <= Integer.MAX_VALUE ? new I((int) nLong) : new L(nLong);
            } else {
                BigInteger nBigInteger = (BigInteger) n;
                return Try.<NonZero>trying(() -> new NonZero.I(nBigInteger.intValueExact()))
                        .catchError(__ -> Try.trying(() -> new NonZero.L(nBigInteger.longValueExact())))
                        .recover(__ -> new NonZero.B(nBigInteger));
            }
        }

        static final class I extends NonZero {
            private static final I ONE = new I(1);

            private final int value;

            I(int value) {
                this.value = value;
            }

            @Override
            public NonZero plus(Natural addend) {
                return Try.<NonZero>trying(() -> new I(addExact(value, addend.intValue())))
                        .catchError(__ -> trying(() -> new L(addExact(value, addend.longValue()))))
                        .catchError(__ -> success(new B(bigIntegerValue().add(addend.bigIntegerValue()))))
                        .orThrow();
            }

            @Override
            public Maybe<Natural> minus(NonZero subtrahend) {
                return subtrahend instanceof I ? natural(value - ((I) subtrahend).value()) : nothing();
            }

            @Override
            public BigInteger bigIntegerValue() {
                return BigInteger.valueOf(value);
            }

            @Override
            public int intValue() {
                return value;
            }

            @Override
            public long longValue() {
                return value;
            }

            @Override
            Integer value() {
                return value;
            }
        }

        static final class L extends NonZero {

            private final long value;

            L(long value) {
                this.value = value;
            }

            @Override
            public NonZero plus(Natural addend) {
                return Try.<NonZero>trying(() -> new L(addExact(value, addend.longValue())))
                        .catchError(__ -> success(new B(bigIntegerValue().add(addend.bigIntegerValue()))))
                        .orThrow();
            }

            @Override
            public Maybe<Natural> minus(NonZero subtrahend) {
                return subtrahend instanceof I || subtrahend instanceof L
                       ? natural(value - subtrahend.longValue())
                       : nothing();
            }

            @Override
            public BigInteger bigIntegerValue() {
                return BigInteger.valueOf(value);
            }

            @Override
            public int intValue() {
                return (int) min(Integer.MAX_VALUE, value);
            }

            @Override
            public long longValue() {
                return value;
            }

            @Override
            Long value() {
                return value;
            }
        }

        static final class B extends NonZero {

            private final BigInteger value;

            B(BigInteger value) {
                this.value = value;
            }

            @Override
            public NonZero.B plus(Natural addend) {
                return new B(value.add(addend.bigIntegerValue()));
            }

            @Override
            public Maybe<Natural> minus(NonZero subtrahend) {
                return natural(value.subtract(subtrahend.bigIntegerValue()));
            }

            @Override
            public BigInteger bigIntegerValue() {
                return value;
            }

            @Override
            public int intValue() {
                return Try.trying(value::intValueExact)
                        .catching(ArithmeticException.class, constantly(Integer.MAX_VALUE))
                        .orThrow();
            }

            @Override
            public long longValue() {
                return Try.trying(value::longValueExact)
                        .catching(ArithmeticException.class, constantly(Long.MAX_VALUE))
                        .orThrow();
            }

            @Override
            BigInteger value() {
                return value;
            }
        }
    }
}
