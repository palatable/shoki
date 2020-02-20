package com.jnape.palatable.shoki.internal;

import com.jnape.palatable.lambda.adt.Maybe;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Flatten.flatten;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Cons.cons;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Drop.drop;
import static com.jnape.palatable.lambda.functions.builtin.fn2.InGroupsOf.inGroupsOf;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Intersperse.intersperse;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Snoc.snoc;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Take.take;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.functions.builtin.fn3.Times.times;
import static com.jnape.palatable.lambda.monoid.builtin.Concat.concat;
import static com.jnape.palatable.shoki.internal.Bitmap32.Bit.ONE;
import static com.jnape.palatable.shoki.internal.Bitmap32.Bit.ZERO;
import static java.lang.Integer.MIN_VALUE;
import static java.lang.String.join;
import static java.util.Arrays.asList;
import static java.util.Collections.singleton;

public final class Bitmap32 {
    private static final Bitmap32 EMPTY = bitmap32(0);

    private final int bits;

    private Bitmap32(int bits) {
        this.bits = bits;
    }

    public int bits() {
        return bits;
    }

    public int populationCount() {
        return Integer.bitCount(bits);
    }

    public Bitmap32 lowerBits(int index) {
        return index <= 0
               ? empty()
               : index >= 32
                 ? this
                 : bitmap32(bits & (-1 >>> 32 - index));
    }

    public Bit lsb() {
        return bits >> 31 == 0 ? ZERO : ONE;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof Bitmap32 && bits == ((Bitmap32) other).bits;
    }

    @Override
    public int hashCode() {
        return bits;
    }

    @Override
    public String toString() {
        Iterable<String> bitStrings = map(Objects::toString, asList(Bit.bits(bits)));
        return join("", concat(cons("0b", snoc("_", take(2, bitStrings))),
                               flatten(intersperse(singleton("_"), inGroupsOf(5, drop(2, bitStrings))))));
    }

    public boolean populatedAtIndex(int index) {
        return (bits & (1 << index)) >>> index == 1;
    }

    public Bitmap32 populateAtIndex(int index) {
        return new Bitmap32(bits | 1 << index);
    }

    public Bitmap32 evictAtIndex(int index) {
        return new Bitmap32(bits & ~(1 << index));
    }

    public Bitmap32 and(Bitmap32 mask) {
        return bitmap32(bits & mask.bits);
    }

    public Bitmap32 signedRightShift(int positions) {
        return bitmap32(bits >>> positions);
    }

    public static Bitmap32 bitmap32(int bits) {
        return new Bitmap32(bits);
    }

    public static Maybe<Bitmap32> fromBits(Bit... bits) {
        if (bits.length > 32)
            return nothing();

        return just(bitmap32(foldLeft((maskWithIndex, bitAtIndex) -> maskWithIndex
                                              .into((mask, index) -> tuple(mask | (bitAtIndex.ordinal() << index),
                                                                           index - 1)),
                                      tuple(0, bits.length - 1),
                                      asList(bits))
                                     ._1()));
    }

    public static Bitmap32 empty() {
        return EMPTY;
    }

    public enum Bit {
        ZERO,
        ONE;

        @Override
        public String toString() {
            return Integer.toString(ordinal());
        }

        public static Bit[] bits(int bitmap) {
            Bit[] bits = new Bit[32];
            times(32, shift -> {
                bits[shift] = (bitmap & (MIN_VALUE >>> shift)) == 0 ? ZERO : ONE;
                return shift + 1;
            }, 0);
            return bits;
        }
    }
}
