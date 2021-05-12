package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.testsupport.DefaultMethodsSet;
import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.referenceEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.identityHashCode;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class HashSetTest {

    @Test
    public void add() {
        assertEquals(hashSet("foo"), HashSet.<String>hashSet().add("foo"));
        assertEquals(hashSet("foo"), HashSet.<String>hashSet().add("foo").add("foo"));
        assertEquals(hashSet("foo", "bar"), HashSet.<String>hashSet().add("foo").add("bar"));
    }

    @Test
    public void remove() {
        assertEquals(HashSet.<String>hashSet(), HashSet.<String>hashSet().remove("foo"));
        assertEquals(HashSet.<String>hashSet(), HashSet.<String>hashSet().add("foo").remove("foo"));
        assertEquals(hashSet("bar"), HashSet.<String>hashSet().add("foo").add("bar").remove("foo"));
    }

    @Test
    public void head() {
        assertEquals(nothing(), hashSet().head());
        assertEquals(just("foo"), HashSet.<String>hashSet().add("foo").head());
        assertEquals(just("foo"), HashSet.<String>hashSet().add("foo").add("bar").head());
        assertEquals(just("foo"), HashSet.<String>hashSet().add("bar").add("foo").head());
    }

    @Test
    public void tail() {
        assertEquals(hashSet(), hashSet().tail());
        assertEquals(hashSet(), HashSet.<String>hashSet().add("foo").tail());
        assertEquals(hashSet("bar"), HashSet.<String>hashSet().add("foo").add("bar").tail());
        assertEquals(hashSet("bar"), HashSet.<String>hashSet().add("bar").add("foo").tail());
    }

    @Test
    public void contains() {
        assertFalse(HashSet.<String>hashSet().contains("foo"));
        assertFalse(HashSet.<String>hashSet().add("bar").contains("foo"));
        assertTrue(HashSet.<String>hashSet().add("foo").contains("foo"));
        assertTrue(HashSet.<String>hashSet().add("foo").add("bar").contains("foo"));
        assertFalse(HashSet.<String>hashSet().add("foo").remove("foo").contains("foo"));
    }

    @Test
    public void isEmpty() {
        assertTrue(hashSet().isEmpty());
        assertFalse(HashSet.<String>hashSet().add("foo").isEmpty());
        assertTrue(HashSet.<String>hashSet().add("foo").remove("foo").isEmpty());
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(zero()), hashSet().sizeInfo());
        assertEquals(known(one()), HashSet.<String>hashSet().add("foo").sizeInfo());
        assertEquals(known(one()), HashSet.<String>hashSet().add("foo").add("foo").sizeInfo());
        assertEquals(known(abs(2)), HashSet.<String>hashSet().add("foo").add("bar").sizeInfo());
        assertEquals(known(zero()), HashSet.<String>hashSet().add("foo").remove("foo").sizeInfo());
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(hashSet(), hashSet());
        assertNotEquals(hashSet(), HashSet.<String>hashSet().add("foo"));
        assertEquals(HashSet.<String>hashSet(), HashSet.<String>hashSet().add("foo").remove("foo"));
        assertEquals(HashSet.<String>hashSet().add("foo"), HashSet.<String>hashSet().add("foo"));
        assertNotEquals(HashSet.<String>hashSet().add("foo"), HashSet.<String>hashSet().add("bar"));
        assertNotEquals(HashSet.<String>hashSet().add("foo"), new Object());

        assertEquals(hashSet().hashCode(), hashSet().hashCode());
        assertNotEquals(hashSet().hashCode(), hashSet("foo").hashCode());
        assertNotEquals(hashSet("foo").hashCode(), hashSet("bar").hashCode());
        assertEquals(hashSet("foo").hashCode(), hashSet("foo", "foo").hashCode());
    }

    @Test
    public void usefulToString() {
        assertEquals("HashSet[]", hashSet().toString());
        assertEquals("HashSet[foo, bar, baz]", HashSet.<String>hashSet().add("foo").add("bar").add("baz").toString());
        assertEquals("HashSet[foo, bar, baz]", HashSet.<String>hashSet().add("bar").add("baz").add("foo").toString());
    }

    @Test
    public void emptySingleton() {
        assertSame(hashSet(), hashSet());
    }

    @Test
    public void customEquivalenceRelationAndHashCode() {
        Integer saboteur = 666;
        HashSet<Integer> identityHashSet = HashSet.<Integer>hashSet(referenceEquals(), identityHashCode())
                .add(1)
                .add(saboteur);
        assertTrue(identityHashSet.contains(1));
        assertFalse(identityHashSet.contains(666));
        assertTrue(identityHashSet.contains(saboteur));

        assertEquals(identityHashSet, hashSet(referenceEquals(), identityHashCode(), 1, saboteur));
        assertNotEquals(hashSet(1, 666), identityHashSet);
        assertEquals(identityHashSet, hashSet(1, 666));
    }

    @Test
    public void addAll() {
        assertEquals(hashSet(), hashSet().addAll(strictStack()));
        assertEquals(hashSet(1, 2, 3), hashSet().addAll(strictStack(1, 2, 3)));
        assertEquals(hashSet(1, 2, 3), hashSet(1, 2, 3).addAll(strictStack()));
        assertEquals(hashSet(1, 2, 3), hashSet(1, 2, 3).addAll(strictStack(1, 2, 3)));
        assertEquals(hashSet(1, 2, 3, 4), hashSet(1, 2, 3).addAll(strictStack(2, 3, 4)));
        assertEquals(hashSet(1, 2, 3, 4), hashSet(2, 3, 4).addAll(strictStack(1, 2, 3)));
    }

    @Test
    public void intersection() {
        assertEquals(hashSet(), hashSet().intersection(hashSet()));
        assertEquals(hashSet(), hashSet().intersection(hashSet(1, 2, 3)));
        assertEquals(hashSet(), hashSet(1, 2, 3).intersection(hashSet()));
        assertEquals(hashSet(1, 2, 3), hashSet(1, 2, 3).intersection(hashSet(1, 2, 3)));
        assertEquals(hashSet(2, 3), hashSet(1, 2, 3).intersection(hashSet(2, 3, 4)));
        assertEquals(hashSet(2, 3), hashSet(2, 3, 4).intersection(hashSet(1, 2, 3)));
    }

    @Test
    public void union() {
        assertEquals(hashSet(), hashSet().union(hashSet()));
        assertEquals(hashSet(1, 2, 3), hashSet().union(hashSet(1, 2, 3)));
        assertEquals(hashSet(1, 2, 3), hashSet(1, 2, 3).union(hashSet()));
        assertEquals(hashSet(1, 2, 3), hashSet(1, 2, 3).union(hashSet(1, 2, 3)));
        assertEquals(hashSet(1, 2, 3, 4), hashSet(1, 2, 3).union(hashSet(2, 3, 4)));
        assertEquals(hashSet(1, 2, 3, 4), hashSet(2, 3, 4).union(hashSet(1, 2, 3)));
    }

    @Test
    public void difference() {
        assertEquals(hashSet(), hashSet().difference(hashSet()));
        assertEquals(hashSet(), hashSet().difference(hashSet(1, 2, 3)));
        assertEquals(hashSet(1, 2, 3), hashSet(1, 2, 3).difference(hashSet()));
        assertEquals(hashSet(), hashSet(1, 2, 3).difference(hashSet(1, 2, 3)));
        assertEquals(hashSet(1), hashSet(1, 2, 3).difference(hashSet(2, 3, 4)));
        assertEquals(hashSet(4), hashSet(2, 3, 4).difference(hashSet(1, 2, 3)));
    }

    @Test
    public void symmetricDifference() {
        HashSet<Object> empty = hashSet();
        assertThat(DefaultMethodsSet.delegate(empty).symmetricDifference(empty),
                   equivalentTo(empty.symmetricDifference(empty), sameElements()));

    }
}