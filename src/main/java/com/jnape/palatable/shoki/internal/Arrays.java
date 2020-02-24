package com.jnape.palatable.shoki.internal;

import static java.lang.Math.max;
import static java.lang.System.arraycopy;

public final class Arrays {
    private Arrays() {
    }

    public static Object[] insertAt(int index, Object[] array, Object value) {
        Object[] copy     = new Object[max(array.length, index) + 1];
        int      maxBound = array.length;
        if (index == 0) {
            arraycopy(array, 0, copy, 1, maxBound);
        } else if (index == maxBound) {
            arraycopy(array, 0, copy, 0, maxBound);
        } else {
            arraycopy(array, 0, copy, 0, index);
            arraycopy(array, index, copy, index + 1, maxBound - index);
        }
        copy[index] = value;
        return copy;
    }

    public static Object[] deleteAt(int index, Object[] array) {
        Object[] copy     = new Object[array.length - 1];
        int      maxBound = copy.length;
        if (index == 0) {
            arraycopy(array, 1, copy, 0, maxBound);
        } else if (index == maxBound) {
            arraycopy(array, 0, copy, 0, maxBound);
        } else {
            arraycopy(array, 0, copy, 0, index);
            arraycopy(array, index + 1, copy, index, maxBound - index);
        }
        return copy;
    }

    public static Object[] overrideAt(int index, Object[] array, Object value) {
        Object[] copy     = new Object[array.length];
        int      maxBound = copy.length - 1;
        copy[index] = value;
        if (index == 0) {
            arraycopy(array, 1, copy, 1, maxBound);
        } else if (index == maxBound) {
            arraycopy(array, 0, copy, 0, maxBound);
        } else {
            arraycopy(array, 0, copy, 0, index);
            arraycopy(array, index + 1, copy, index + 1, maxBound - index);
        }
        return copy;
    }
}
