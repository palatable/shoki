package com.jnape.palatable.shoki.impl;

import java.util.Objects;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Flatten.flatten;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Cons.cons;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Drop.drop;
import static com.jnape.palatable.lambda.functions.builtin.fn2.InGroupsOf.inGroupsOf;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Intersperse.intersperse;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Snoc.snoc;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Take.take;
import static com.jnape.palatable.lambda.functions.builtin.fn3.Times.times;
import static com.jnape.palatable.lambda.monoid.builtin.Concat.concat;
import static java.lang.Integer.MIN_VALUE;
import static java.lang.String.join;
import static java.util.Arrays.asList;
import static java.util.Collections.singleton;

final class Bitmap32 {

    private Bitmap32() {
    }

    public static String toString(int bits) {
        Iterable<String> bitStrings = map(Objects::toString, asList(Bit.bits(bits)));
        return join("", concat(cons("0b", snoc("_", take(2, bitStrings))),
                               flatten(intersperse(singleton("_"), inGroupsOf(5, drop(2, bitStrings))))));
    }

    public static int evictAtIndex(int bits, int index) {
        return bits & ~(1 << index);
    }

    public static int populateAtIndex(int bits, int index) {
        return bits | (1 << index);
    }

    public static boolean populatedAtIndex(int bits, int index) {
        return ((bits & (1 << index)) >>> index) == 1;
    }

    public static int lowerBits(int bits, int index) {
        return index <= 0 ? 0 : index >= 32 ? bits : bits & (-1 >>> (32 - index));
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
