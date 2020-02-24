package com.jnape.palatable.shoki.internal;

import static java.lang.System.arraycopy;

public final class DenseArrays {
    private DenseArrays() {
    }

    public static Object[] insertAt(int index, Object[] arr, Object value) {
        Object[] copy     = new Object[arr.length + 1];
        int      maxBound = arr.length;
        if (index == 0) {
            arraycopy(arr, 0, copy, 1, maxBound);
        } else if (index == maxBound) {
            arraycopy(arr, 0, copy, 0, maxBound);
        } else {
            arraycopy(arr, 0, copy, 0, index);
            arraycopy(arr, index, copy, index + 1, maxBound - index);
        }
        copy[index] = value;
        return copy;
    }

    public static Object[] deleteAt(int index, Object[] arr) {
        Object[] copy     = new Object[arr.length - 1];
        int      maxBound = copy.length;
        if (index == 0) {
            arraycopy(arr, 1, copy, 0, maxBound);
        } else if (index == maxBound) {
            arraycopy(arr, 0, copy, 0, maxBound);
        } else {
            arraycopy(arr, 0, copy, 0, index);
            arraycopy(arr, index + 1, copy, index, maxBound - index);
        }
        return copy;
    }

    public static Object[] overrideAt(int index, Object[] arr, Object value) {
        Object[] copy     = new Object[arr.length];
        int      maxBound = copy.length - 1;
        copy[index] = value;
        if (index == 0) {
            arraycopy(arr, 1, copy, 1, maxBound);
        } else if (index == maxBound) {
            arraycopy(arr, 0, copy, 0, maxBound);
        } else {
            arraycopy(arr, 0, copy, 0, index);
            arraycopy(arr, index + 1, copy, index + 1, maxBound - index);
        }
        return copy;
    }
}
