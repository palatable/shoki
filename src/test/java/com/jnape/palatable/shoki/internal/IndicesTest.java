package com.jnape.palatable.shoki.internal;

import org.junit.Test;

import static com.jnape.palatable.shoki.internal.Bitmap32.bitmap32;
import static org.junit.Assert.assertEquals;

public class IndicesTest {

    @Test
    public void entryMapIndex() {
        assertEquals(31, Indices.bitmapIndex(bitmap32(0b00_00000_00000_00000_00000_00000_11111), 1));
        assertEquals(0, Indices.bitmapIndex(bitmap32(0b00_00000_00000_00000_00000_00000_11111), 2));
        assertEquals(31, Indices.bitmapIndex(bitmap32(0b00_00000_00000_00000_00000_11111_00000), 2));
        assertEquals(21, Indices.bitmapIndex(bitmap32(0b00_00000_00000_00000_10101_00000_00000), 3));
        assertEquals(3, Indices.bitmapIndex(bitmap32(0b11_11111_11111_11111_11111_11111_11111), 7));
    }

    @Test
    public void tableIndex() {
        assertEquals(0, Indices.tableIndex(bitmap32(0b00_00000_00000_00000_00000_00000_11111), 0));
        assertEquals(1, Indices.tableIndex(bitmap32(0b00_00000_00000_00000_00000_00000_11111), 1));
        assertEquals(3, Indices.tableIndex(bitmap32(0b00_00000_00000_00000_00000_00000_11111), 3));
        assertEquals(5, Indices.tableIndex(bitmap32(0b00_00000_00000_00000_00000_00000_11111), 10));
        assertEquals(32, Indices.tableIndex(bitmap32(0b11_11111_11111_11111_11111_11111_11111), 32));
        assertEquals(32, Indices.tableIndex(bitmap32(0b11_11111_11111_11111_11111_11111_11111), 35));
    }
}