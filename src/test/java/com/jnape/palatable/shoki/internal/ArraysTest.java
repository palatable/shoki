package com.jnape.palatable.shoki.internal;

import org.junit.Test;

import java.util.Arrays;

import static com.jnape.palatable.shoki.internal.Arrays.*;
import static org.junit.Assert.*;

public class ArraysTest {

    @Test
    public void insertAtMaintainsDensityGuarantee() {
        assertArrayEquals(new Object[]{"foo"}, insertAt(0, new Object[0], "foo"));
        assertArrayEquals(new Object[]{"foo", "bar"}, insertAt(0, new Object[]{"bar"}, "foo"));
        assertArrayEquals(new Object[]{"bar", "foo"}, insertAt(1, new Object[]{"bar"}, "foo"));
        assertArrayEquals(new Object[]{"foo", "bar", "baz"}, insertAt(1, new Object[]{"foo", "baz"}, "bar"));
    }

    @Test
    public void insertAtCopies() {
        Object[] original = new Object[0];
        insertAt(0, original, "foo");
        assertEquals(0, original.length);
    }

    @Test(expected = ArrayIndexOutOfBoundsException.class)
    public void insertBeyondLastElementPlusOneWouldViolateDensityGuarantees() {
        insertAt(1, new Object[0], "anything");
    }

    @Test
    public void deleteAtMaintainsDensityGuarantee() {
        assertArrayEquals(new Object[0], deleteAt(0, new Object[]{"foo"}));
        assertArrayEquals(new Object[]{"bar"}, deleteAt(0, new Object[]{"foo", "bar"}));
        assertArrayEquals(new Object[]{"foo"}, deleteAt(1, new Object[]{"foo", "bar"}));
        assertArrayEquals(new Object[]{"foo", "baz"}, deleteAt(1, new Object[]{"foo", "bar", "baz"}));
    }

    @Test
    public void deleteAtCopies() {
        Object[] original = new Object[]{"anything"};
        deleteAt(0, original);
        assertEquals(1, original.length);
    }

    @Test
    public void overrideAtReplacesValueAtSlot() {
        assertArrayEquals(new Object[]{"bar"}, overrideAt(0, new Object[]{"foo"}, "bar"));
        assertArrayEquals(new Object[]{"bar", "baz"}, overrideAt(0, new Object[]{"foo", "baz"}, "bar"));
        assertArrayEquals(new Object[]{"foo", "bar"}, overrideAt(1, new Object[]{"foo", "baz"}, "bar"));
        assertArrayEquals(new Object[]{"foo", "bar", "baz"}, overrideAt(1, new Object[]{"foo", "qux", "baz"}, "bar"));
    }

    @Test
    public void overrideAtCopies() {
        Object[] original = new Object[]{"anything"};
        Object[] updated  = overrideAt(0, original, "anything else");
        assertFalse(Arrays.equals(original, updated));
    }

    @Test(expected = ArrayIndexOutOfBoundsException.class)
    public void overrideAtCannotInsert() {
        overrideAt(0, new Object[0], "anything");
    }
}