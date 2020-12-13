package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static java.lang.Integer.MAX_VALUE;
import static java.lang.Integer.MIN_VALUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class Bitmap32Test {

    @Test
    public void bitIsSet() {
        assertFalse(Bitmap32.bitIsSet(0, 0));
        assertTrue(Bitmap32.bitIsSet(1, 0));
        assertFalse(Bitmap32.bitIsSet(1, 1));
        assertTrue(Bitmap32.bitIsSet(3, 1));
        assertFalse(Bitmap32.bitIsSet(MAX_VALUE, 31));
        assertTrue(Bitmap32.bitIsSet(MIN_VALUE, 31));
        assertTrue(Bitmap32.bitIsSet(-1, 31));
    }

    @Test
    public void setBit() {
        assertEquals(1, Bitmap32.setBit(0, 0));
        assertEquals(2, Bitmap32.setBit(2, 1));
        assertEquals(3, Bitmap32.setBit(1, 1));
        assertEquals(MIN_VALUE, Bitmap32.setBit(0, 31));
        assertEquals(-1, Bitmap32.setBit(MAX_VALUE, 31));
    }

    @Test
    public void unsetBit() {
        assertEquals(0, Bitmap32.unsetBit(1, 0));
        assertEquals(1, Bitmap32.unsetBit(1, 1));
        assertEquals(2, Bitmap32.unsetBit(3, 0));
        assertEquals(MAX_VALUE, Bitmap32.unsetBit(-1, 31));
    }

    @Test
    public void lowerBits() {
        assertEquals(0, Bitmap32.lowerBits(0, 10));
        assertEquals(0b00_00000_00000_00000_00000_00000_00000,
                     Bitmap32.lowerBits(0b00_00000_00000_00000_00000_00000_00001, 0));
        assertEquals(0b00_00000_00000_00000_00000_00000_00001,
                     Bitmap32.lowerBits(0b00_00000_00000_00000_00000_00000_00011, 1));
        assertEquals(0b00_00000_00000_00000_00000_00000_01111,
                     Bitmap32.lowerBits(0b00_00000_00000_00000_00000_00000_11111, 4));
    }
}