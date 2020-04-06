package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsMultiSet;
import org.junit.Test;

import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class MultiSetTest {

    @Test
    public void removeAll() {
        assertTrue(DefaultMethodsMultiSet.<String>delegate(HashMultiSet.empty()).removeAll("foo").isEmpty());
        assertEquals(zero(),
                     DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).removeAll("foo").get("foo"));
        assertEquals(known(one()),
                     DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).removeAll("foo").sizeInfo());
    }

    @Test
    public void removeOne() {
        assertTrue(DefaultMethodsMultiSet.<String>delegate(HashMultiSet.empty()).remove("foo").isEmpty());
        assertEquals(one(), DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "foo")).remove("foo").get("foo"));
        assertEquals(known(abs(2)),
                     DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).remove("foo").sizeInfo());
    }

    @Test
    public void addOne() {
        assertEquals(one(), DefaultMethodsMultiSet.delegate(HashMultiSet.<String>empty()).add("foo").get("foo"));
    }

    @Test
    public void addAll() {
        assertTrue(DefaultMethodsMultiSet.<String>delegate(HashMultiSet.empty())
                           .addAll(HashMultiSet.empty()).isEmpty());

        MultiSet<String> addAll = DefaultMethodsMultiSet.<String>delegate(HashMultiSet.empty())
                .addAll(HashMultiSet.of("foo", "foo", "bar"));
        assertEquals(abs(2), addAll.get("foo"));
        assertEquals(abs(1), addAll.get("bar"));
        assertEquals(known(abs(3)), addAll.sizeInfo());
    }
}