package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static java.lang.Integer.MAX_VALUE;
import static java.lang.Integer.MIN_VALUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class Bitmap32Test {

    @Test
    public void friendlyToString() {
        assertEquals("0b00_00000_00000_00000_00000_00000_00000",
                     Bitmap32.toString(0));
        assertEquals("0b00_00000_00000_00000_00000_00000_00001",
                     Bitmap32.toString(1));
        assertEquals("0b11_11111_11111_11111_11111_11111_11111",
                     Bitmap32.toString(-1));
        assertEquals("0b10_00000_00000_00000_00000_00000_00000",
                     Bitmap32.toString(MIN_VALUE));
        assertEquals("0b01_11111_11111_11111_11111_11111_11111",
                     Bitmap32.toString(MAX_VALUE));
    }

    @Test
    public void populatedAtIndex() {
        assertFalse(Bitmap32.populatedAtIndex(0, 0));
        assertTrue(Bitmap32.populatedAtIndex(1, 0));
        assertFalse(Bitmap32.populatedAtIndex(1, 1));
        assertTrue(Bitmap32.populatedAtIndex(3, 1));
        assertFalse(Bitmap32.populatedAtIndex(MAX_VALUE, 31));
        assertTrue(Bitmap32.populatedAtIndex(MIN_VALUE, 31));
        assertTrue(Bitmap32.populatedAtIndex(-1, 31));
    }

    @Test
    public void populateAtIndex() {
        assertEquals(1, Bitmap32.populateAtIndex(0, 0));
        assertEquals(2, Bitmap32.populateAtIndex(2, 1));
        assertEquals(3, Bitmap32.populateAtIndex(1, 1));
        assertEquals(MIN_VALUE, Bitmap32.populateAtIndex(0, 31));
        assertEquals(-1, Bitmap32.populateAtIndex(MAX_VALUE, 31));
    }

    @Test
    public void evictAtIndex() {
        assertEquals(0, Bitmap32.evictAtIndex(1, 0));
        assertEquals(1, Bitmap32.evictAtIndex(1, 1));
        assertEquals(2, Bitmap32.evictAtIndex(3, 0));
        assertEquals(MAX_VALUE, Bitmap32.evictAtIndex(-1, 31));
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
        assertEquals(0b11_11111_11111_11111_11111_11111_11111,
                     Bitmap32.lowerBits(0b11_11111_11111_11111_11111_11111_11111, 32));
        assertEquals(0b11_11111_11111_11111_11111_11111_11111,
                     Bitmap32.lowerBits(0b11_11111_11111_11111_11111_11111_11111, 34));
    }
}