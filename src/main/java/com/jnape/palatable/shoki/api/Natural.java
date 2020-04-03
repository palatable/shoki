package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn1;

import java.math.BigInteger;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Id.id;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Eq.eq;
import static com.jnape.palatable.lambda.functions.builtin.fn2.GTE.gte;
import static com.jnape.palatable.lambda.functions.builtin.fn4.IfThenElse.ifThenElse;
import static java.math.BigInteger.ZERO;

/**
 * A {@link Number} type representing the
 * <a href="https://en.wikipedia.org/wiki/Natural_number" target="_new">natural</a> numbers - the set of non-negative
 * integers - with type-safe interfaces for dealing with the {@link CoProduct2 coproduct} of {@link Zero zero} and
 * {@link NonZero non-zero} values.
 */
public abstract class Natural extends Number implements CoProduct2<Natural.Zero, Natural.NonZero, Natural>, Comparable<Natural> {

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
        return natural(bigIntegerValue().subtract(subtrahend.bigIntegerValue()));
    }

    /**
     * Specialized addition when the addend is {@link NonZero}, guaranteeing a {@link NonZero} sum.
     *
     * @param addend the {@link Natural} addend
     * @return the {@link NonZero} sum
     */
    public final NonZero plus(NonZero addend) {
        return match(constantly(addend), augend -> new NonZero(augend.bigIntegerValue().add(addend.bigIntegerValue())));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final int intValue() {
        return bigIntegerValue().intValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final long longValue() {
        return bigIntegerValue().longValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final float floatValue() {
        return bigIntegerValue().floatValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final double doubleValue() {
        return bigIntegerValue().doubleValue();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Natural other) {
        return bigIntegerValue().compareTo(other.bigIntegerValue());
    }

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
        return NonZero.ONE;
    }

    /**
     * If <code>value</code> is non-negative, return {@link Maybe#just(Object) just} the corresponding {@link Natural};
     * otherwise, return {@link Maybe#nothing() nothing}.
     *
     * @param value the value
     * @return {@link Maybe} the corresponding {@link Natural}
     * @see Natural#natural(long)
     */
    public static Maybe<Natural> natural(BigInteger value) {
        return just(value)
                .filter(gte(ZERO))
                .fmap(ifThenElse(eq(ZERO), constantly(zero()), NonZero::new));
    }

    /**
     * Convenience overload of {@link Natural#natural(BigInteger)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return {@link Maybe} the corresponding {@link Natural}
     * @see Natural#natural(BigInteger)
     */
    public static Maybe<Natural> natural(long value) {
        return natural(BigInteger.valueOf(value));
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
        return natural(value).orElseGet(() -> new NonZero(value.negate()));
    }

    /**
     * Convenience overload of {@link Natural#abs(BigInteger)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the value's absolute value
     * @see Natural#abs(BigInteger)
     */
    public static Natural abs(long value) {
        return abs(BigInteger.valueOf(value));
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
        return natural(value).orElse(zero());
    }

    /**
     * Convenience overload of {@link Natural#clampZero(BigInteger)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return the {@link Natural} corresponding to the given value, or {@link Zero}
     * @see Natural#clampZero(BigInteger)
     */
    public static Natural clampZero(long value) {
        return clampZero(BigInteger.valueOf(value));
    }

    /**
     * Return the corresponding {@link NonZero non-zero} {@link Natural} for the given <code>value</code>, defaulting to
     * {@link Natural#one() one} if <code>value</code> is {@link BigInteger#ZERO zero} or negative.
     *
     * @param value the value
     * @return the {@link NonZero non-zero} {@link Natural} corresponding to the given value, or
     * {@link Natural#one() one}
     * @see Natural#clampOne(long)
     */
    public static NonZero clampOne(BigInteger value) {
        return clampZero(value).match(constantly(one()), id());
    }

    /**
     * Convenience overload of {@link Natural#clampOne(BigInteger)} allowing <code>long</code> values.
     *
     * @param value the value
     * @return the {@link NonZero non-zero} {@link Natural} corresponding to the given value, or
     * {@link Natural#one() one}
     * @see Natural#clampOne(BigInteger)
     */
    public static NonZero clampOne(long value) {
        return clampOne(BigInteger.valueOf(value));
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
        public BigInteger bigIntegerValue() {
            return ZERO;
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
    public static final class NonZero extends Natural {

        private static final NonZero ONE = new NonZero(BigInteger.valueOf(1L));

        private final BigInteger value;

        NonZero(BigInteger value) {
            this.value = value;
        }

        @Override
        public NonZero minus(Zero subtrahend) {
            return this;
        }

        @Override
        public NonZero plus(Natural addend) {
            return new NonZero(value.add(addend.bigIntegerValue()));
        }

        @Override
        public final BigInteger bigIntegerValue() {
            return value;
        }

        @Override
        public <R> R match(Fn1<? super Zero, ? extends R> aFn, Fn1<? super NonZero, ? extends R> bFn) {
            return bFn.apply(this);
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof NonZero && Objects.equals(value, ((NonZero) other).value);
        }

        @Override
        public int hashCode() {
            return Objects.hashCode(value);
        }

        @Override
        public String toString() {
            return "NonZero{value=" + value + '}';
        }
    }
}
