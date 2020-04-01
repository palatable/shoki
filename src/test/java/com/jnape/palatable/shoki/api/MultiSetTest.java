package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsMultiSet;
import org.junit.Test;

import java.math.BigInteger;

import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static java.math.BigInteger.ONE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class MultiSetTest {

    @Test
    public void removeAll() {
        assertTrue(DefaultMethodsMultiSet.<String>delegate(HashMultiSet.empty()).removeAll("foo").isEmpty());
        assertEquals(zero(), DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).removeAll("foo").get("foo"));
        assertEquals(known(ONE),
                     DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).removeAll("foo").sizeInfo());
    }

    @Test
    public void removeOne() {
        assertTrue(DefaultMethodsMultiSet.<String>delegate(HashMultiSet.empty()).remove("foo").isEmpty());
        assertEquals(one(), DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "foo")).remove("foo").get("foo"));
        assertEquals(known(BigInteger.valueOf(2)),
                     DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).remove("foo").sizeInfo());
    }

    @Test
    public void addOne() {
        assertEquals(one(), DefaultMethodsMultiSet.delegate(HashMultiSet.<String>empty()).add("foo").get("foo"));
    }
}