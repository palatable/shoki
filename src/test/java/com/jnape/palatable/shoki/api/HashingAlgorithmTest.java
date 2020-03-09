package com.jnape.palatable.shoki.api;

import org.junit.Test;

import java.util.Objects;

import static org.junit.Assert.assertEquals;

public class HashingAlgorithmTest {

    @Test
    public void objectHashCode() {
        HashingAlgorithm<Object> objectHashCode = HashingAlgorithm.objectHashCode();
        Object                   obj            = new Object();
        assertEquals((Integer) Objects.hashCode(obj), objectHashCode.apply(obj));
    }

    @Test
    public void identityHashCode() {
        HashingAlgorithm<Object> identityHashCode = HashingAlgorithm.identityHashCode();
        Object                   obj              = new Object();
        assertEquals((Integer) System.identityHashCode(obj), identityHashCode.apply(obj));
    }
}