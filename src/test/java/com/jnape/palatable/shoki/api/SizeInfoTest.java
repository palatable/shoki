package com.jnape.palatable.shoki.api;

import org.junit.Test;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.api.SizeInfo.unknown;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;

public class SizeInfoTest {

    @Test
    public void knownSize() {
        assertEquals((Integer) 1, known(1).getSize());
    }

    @Test
    public void unknownSingleton() {
        assertSame(unknown(), unknown());
    }

    @Test
    public void match() {
        assertEquals((Integer) 1, known(1).match(SizeInfo.Known::getSize, constantly(-1)));
        assertEquals((Integer) (-1), SizeInfo.unknown().match(SizeInfo.Known::getSize, constantly(-1)));
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(known(1), known(1));
        assertNotEquals(known(1), known(2));
        assertNotEquals(known(1), known(1F));
        assertNotEquals(known(1), unknown());
        assertNotEquals(known(1), new Object());
        assertNotEquals(unknown(), new Object());

        assertEquals(known(1).hashCode(), known(1).hashCode());
        assertEquals(unknown().hashCode(), unknown().hashCode());
        assertNotEquals(known(1).hashCode(), known(2).hashCode());
        assertNotEquals(known(1).hashCode(), unknown().hashCode());
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("Known{size=1}", known(1).toString());
        assertEquals("Unknown{}", unknown().toString());
    }
}