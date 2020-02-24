package com.jnape.palatable.shoki.internal;

public final class Indices {
    private Indices() {
    }

    public static int bitmapIndex(Bitmap32 bitmap32, int level) {
        int shift = (level - 1) * 5;
        int mask  = 31 << shift;
        return (bitmap32.bits() & mask) >>> shift;
    }

    public static int tableIndex(Bitmap32 bitmap, int bitmapIndex) {
        return bitmap.lowerBits(bitmapIndex).populationCount();
    }
}
