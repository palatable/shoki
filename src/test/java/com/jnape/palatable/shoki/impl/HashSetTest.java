package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.referenceEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.identityHashCode;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

public class HashSetTest {

    @Test
    public void add() {
        assertEquals(HashSet.of("foo"), HashSet.<String>empty().add("foo"));
        assertEquals(HashSet.of("foo"), HashSet.<String>empty().add("foo").add("foo"));
        assertEquals(HashSet.of("foo", "bar"), HashSet.<String>empty().add("foo").add("bar"));
    }

    @Test
    public void remove() {
        assertEquals(HashSet.<String>empty(), HashSet.<String>empty().remove("foo"));
        assertEquals(HashSet.<String>empty(), HashSet.<String>empty().add("foo").remove("foo"));
        assertEquals(HashSet.of("bar"), HashSet.<String>empty().add("foo").add("bar").remove("foo"));
    }

    @Test
    public void head() {
        assertEquals(nothing(), HashSet.empty().head());
        assertEquals(just("foo"), HashSet.<String>empty().add("foo").head());
        assertEquals(just("foo"), HashSet.<String>empty().add("foo").add("bar").head());
        assertEquals(just("foo"), HashSet.<String>empty().add("bar").add("foo").head());
    }

    @Test
    public void tail() {
        assertEquals(HashSet.empty(), HashSet.empty().tail());
        assertEquals(HashSet.empty(), HashSet.<String>empty().add("foo").tail());
        assertEquals(HashSet.of("bar"), HashSet.<String>empty().add("foo").add("bar").tail());
        assertEquals(HashSet.of("bar"), HashSet.<String>empty().add("bar").add("foo").tail());
    }

    @Test
    public void contains() {
        assertFalse(HashSet.<String>empty().contains("foo"));
        assertFalse(HashSet.<String>empty().add("bar").contains("foo"));
        assertTrue(HashSet.<String>empty().add("foo").contains("foo"));
        assertTrue(HashSet.<String>empty().add("foo").add("bar").contains("foo"));
        assertFalse(HashSet.<String>empty().add("foo").remove("foo").contains("foo"));
    }

    @Test
    public void isEmpty() {
        assertTrue(HashSet.empty().isEmpty());
        assertFalse(HashSet.<String>empty().add("foo").isEmpty());
        assertTrue(HashSet.<String>empty().add("foo").remove("foo").isEmpty());
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(0), HashSet.empty().sizeInfo());
        assertEquals(known(1), HashSet.<String>empty().add("foo").sizeInfo());
        assertEquals(known(1), HashSet.<String>empty().add("foo").add("foo").sizeInfo());
        assertEquals(known(2), HashSet.<String>empty().add("foo").add("bar").sizeInfo());
        assertEquals(known(0), HashSet.<String>empty().add("foo").remove("foo").sizeInfo());
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(HashSet.empty(), HashSet.empty());
        assertNotEquals(HashSet.empty(), HashSet.<String>empty().add("foo"));
        assertEquals(HashSet.<String>empty(), HashSet.<String>empty().add("foo").remove("foo"));
        assertEquals(HashSet.<String>empty().add("foo"), HashSet.<String>empty().add("foo"));
        assertNotEquals(HashSet.<String>empty().add("foo"), HashSet.<String>empty().add("bar"));
        assertNotEquals(HashSet.<String>empty().add("foo"), new Object());

        assertEquals(HashSet.empty().hashCode(), HashSet.empty().hashCode());
        assertNotEquals(HashSet.empty().hashCode(), HashSet.of("foo").hashCode());
        assertNotEquals(HashSet.of("foo").hashCode(), HashSet.of("bar").hashCode());
        assertEquals(HashSet.of("foo").hashCode(), HashSet.of("foo", "foo").hashCode());
    }

    @Test
    public void usefulToString() {
        assertEquals("HashSet[]", HashSet.empty().toString());
        assertEquals("HashSet[foo, bar, baz]", HashSet.<String>empty().add("foo").add("bar").add("baz").toString());
        assertEquals("HashSet[foo, bar, baz]", HashSet.<String>empty().add("bar").add("baz").add("foo").toString());
    }

    @Test
    public void emptySingleton() {
        assertSame(HashSet.empty(), HashSet.empty());
    }

    @Test
    public void customEquivalenceRelationAndHashCode() {
        Integer saboteur = 666;
        HashSet<Integer> identityHashSet = HashSet.<Integer>empty(referenceEquals(), identityHashCode())
                .add(1)
                .add(saboteur);
        assertTrue(identityHashSet.contains(1));
        assertFalse(identityHashSet.contains(666));
        assertTrue(identityHashSet.contains(saboteur));

        assertEquals(identityHashSet, HashSet.of(referenceEquals(), identityHashCode(), 1, saboteur));
        assertNotEquals(identityHashSet, HashSet.of(1, 666));
        assertEquals(HashSet.of(1, 666), identityHashSet);
    }
}