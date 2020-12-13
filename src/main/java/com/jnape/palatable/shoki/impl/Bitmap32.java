package com.jnape.palatable.shoki.impl;

final class Bitmap32 {

    private Bitmap32() {
    }

    static int unsetBit(int bitmap, int index) {
        return bitmap & ~(1 << index);
    }

    static int setBit(int bitmap, int index) {
        return bitmap | (1 << index);
    }

    static boolean bitIsSet(int bitmap, int index) {
        return (bitmap & 1 << index) != 0;
    }

    static int lowerBits(int bitmap, int index) {
        return bitmap & (1 << index) - 1;
    }
}
