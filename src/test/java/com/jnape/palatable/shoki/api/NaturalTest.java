package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.Natural.NonZero;
import com.jnape.palatable.shoki.api.Natural.Zero;
import org.junit.Test;

import java.math.BigInteger;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn2.CmpEq.cmpEq;
import static com.jnape.palatable.lambda.functions.builtin.fn2.GT.gt;
import static com.jnape.palatable.lambda.functions.builtin.fn2.LT.lt;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static java.math.BigInteger.ONE;
import static java.math.BigInteger.TEN;
import static java.math.BigInteger.ZERO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class NaturalTest {

    @Test
    public void value() {
        assertEquals(ZERO, zero().bigIntegerValue());
        assertEquals(ONE, Natural.abs(ONE).projectB().orElseThrow(AssertionError::new).bigIntegerValue());
    }

    @Test
    public void number() {
        Zero zero = zero();
        assertEquals((byte) 0, zero.byteValue());
        assertEquals((short) 0, zero.shortValue());
        assertEquals(0, zero.intValue());
        assertEquals(0L, zero.longValue());
        assertEquals(0F, zero.floatValue(), 0F);
        assertEquals(0D, zero.doubleValue(), 0D);

        NonZero nonZero = new NonZero(ONE);
        assertEquals((byte) 1, nonZero.byteValue());
        assertEquals((short) 1, nonZero.shortValue());
        assertEquals(1, nonZero.intValue());
        assertEquals(1L, nonZero.longValue());
        assertEquals(1F, nonZero.floatValue(), 0F);
        assertEquals(1D, nonZero.doubleValue(), 0D);
    }

    @Test
    public void zeroSingleton() {
        assertSame(zero(), zero());
    }

    @Test
    public void abs() {
        assertEquals(zero(), Natural.abs(ZERO));
        assertEquals(zero(), Natural.abs(ZERO.negate()));
        assertEquals(zero(), Natural.abs(BigInteger.valueOf(0L).negate()));
        assertEquals(zero(), Natural.abs(new BigInteger("0")));
        assertEquals(new NonZero(ONE), Natural.abs(ONE));
        assertEquals(new NonZero(ONE), Natural.abs(ONE.negate()));
        assertEquals(Natural.abs(ONE), Natural.abs(1L));
    }

    @Test
    public void natural() {
        assertEquals(just(zero()), Natural.natural(ZERO));
        assertEquals(just(new NonZero(ONE)), Natural.natural(ONE));
        assertEquals(nothing(), Natural.natural(ONE.negate()));
        assertEquals(Natural.natural(ONE), Natural.natural(1L));
    }

    @Test
    public void clampZero() {
        assertEquals(zero(), Natural.clampZero(ZERO));
        assertEquals(new NonZero(ONE), Natural.clampZero(ONE));
        assertEquals(zero(), Natural.clampZero(ONE.negate()));
        assertEquals(Natural.clampZero(ONE), Natural.clampZero(1L));
    }

    @Test
    public void clampOne() {
        assertEquals(new NonZero(ONE), Natural.clampOne(ZERO));
        assertEquals(new NonZero(ONE), Natural.clampOne(ONE));
        assertEquals(new NonZero(TEN), Natural.clampOne(TEN));
        assertEquals(new NonZero(ONE), Natural.clampOne(ONE.negate()));
        assertEquals(Natural.clampOne(BigInteger.valueOf(2)), Natural.clampZero(2L));
    }

    @Test
    public void addition() {
        Natural zero      = Natural.zero();
        Natural one       = Natural.one();
        Natural stillZero = zero.plus(zero);
        Natural stillOne  = one.plus(zero);
        Natural alsoOne   = zero.plus(one);

        assertEquals(zero, stillZero);
        assertEquals(one, stillOne);
        assertEquals(stillOne, alsoOne);
    }

    @Test
    public void specializedZeroAddition() {
        Zero    zero      = Natural.zero();
        NonZero one       = Natural.one();
        Zero    stillZero = zero.plus(zero);
        NonZero stillOne  = one.plus(zero);
        NonZero alsoOne   = zero.plus(one);
        assertEquals(zero, stillZero);
        assertEquals(one, stillOne);
        assertEquals(stillOne, alsoOne);
    }

    @Test
    public void specializedNonZeroAddition() {
        Zero    zero     = Natural.zero();
        NonZero one      = Natural.one();
        NonZero stillOne = one.plus(zero);
        NonZero alsoOne  = zero.plus(one);
        assertEquals(one, stillOne);
        assertEquals(stillOne, alsoOne);
    }

    @Test
    public void subtraction() {
        Natural        zero         = Natural.zero();
        Natural        one          = Natural.one();
        Maybe<Natural> stillZero    = zero.minus(zero);
        Maybe<Natural> lessThanZero = zero.minus(one);
        Maybe<Natural> stillOne     = one.minus(zero);

        assertEquals(just(zero), stillZero);
        assertEquals(nothing(), lessThanZero);
        assertEquals(just(one), stillOne);
        assertEquals(zero, zero.minus(zero()));
    }

    @Test
    public void specializedZeroSubtraction() {
        Zero           zero         = Natural.zero();
        NonZero        one          = Natural.one();
        Zero           stillZero    = zero.minus(zero);
        Maybe<Natural> lessThanZero = zero.minus(one);

        assertEquals(zero, stillZero);
        assertEquals(nothing(), lessThanZero);
    }

    @Test
    public void specializedNonZeroSubtraction() {
        Zero           zero         = Natural.zero();
        NonZero        one          = Natural.one();
        NonZero        stillOne     = one.minus(zero);
        Maybe<Natural> alsoZero     = one.minus(one);
        Maybe<Natural> lessThanZero = one.minus(one.plus(one));

        assertEquals(one, stillOne);
        assertEquals(just(zero), alsoZero);
        assertEquals(nothing(), lessThanZero);
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(zero(), zero());
        assertEquals(new NonZero(ONE), new NonZero(ONE));
        assertNotEquals(zero(), new NonZero(ONE));
        assertNotEquals(new NonZero(ONE), zero());
        assertNotEquals(new NonZero(ONE), new NonZero(BigInteger.valueOf(2)));
        assertNotEquals(new NonZero(ONE), new Object());

        assertEquals(zero().hashCode(), zero().hashCode());
        assertEquals(new NonZero(ONE).hashCode(), new NonZero(ONE).hashCode());
        assertNotEquals(new NonZero(ONE).hashCode(), new NonZero(BigInteger.valueOf(2)).hashCode());
    }

    @Test
    public void comparison() {
        assertTrue(lt(one(), zero()));
        assertTrue(gt(zero(), one()));
        assertTrue(cmpEq(zero(), zero()));
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("Zero{}", zero().toString());
        assertEquals("NonZero{value=1}", new NonZero(BigInteger.valueOf(1)).toString());
    }
}