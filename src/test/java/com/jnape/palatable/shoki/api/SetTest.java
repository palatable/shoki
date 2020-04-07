package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsSet;
import com.jnape.palatable.shoki.testsupport.StubbedHashingAlgorithm;
import org.junit.Test;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SetTest {

    @Test
    public void equals() {
        assertTrue(Set.equals(HashSet.empty(), HashSet.empty()));
        assertTrue(Set.equals(HashSet.of(1, 2), HashSet.of(1, 2)));
        assertTrue(Set.equals(HashSet.of(1, 2), HashSet.of(2, 1)));
        assertFalse(Set.equals(HashSet.of(1, 2), HashSet.empty()));
        assertFalse(Set.equals(HashSet.empty(), HashSet.of(1, 2)));

        assertTrue(Set.equals(HashSet.of(1, 2, 3),
                              HashSet.of(objectEquals(),
                                         StubbedHashingAlgorithm.<Integer>stubbedHashingAlgorithm()
                                                 .stub(1, 3)
                                                 .stub(2, 2)
                                                 .stub(3, 1),
                                         1, 2, 3)));
    }

    @Test
    public void symmetricDifference() {
        DefaultMethodsSet<Natural, Integer> empty = DefaultMethodsSet.delegate(HashSet.empty());
        DefaultMethodsSet<Natural, Integer> _123  = DefaultMethodsSet.delegate(HashSet.of(1, 2, 3));
        DefaultMethodsSet<Natural, Integer> _234  = DefaultMethodsSet.delegate(HashSet.of(2, 3, 4));
        DefaultMethodsSet<Natural, Integer> _14   = DefaultMethodsSet.delegate(HashSet.of(1, 4));

        assertTrue(Set.equals(empty, empty.symmetricDifference(empty)));
        assertTrue(Set.equals(_123, empty.symmetricDifference(_123)));
        assertTrue(Set.equals(_123, _123.symmetricDifference(empty)));
        assertTrue(Set.equals(empty, _123.symmetricDifference(_123)));
        assertTrue(Set.equals(_14, _123.symmetricDifference(_234)));
        assertTrue(Set.equals(_14, _234.symmetricDifference(_123)));
    }

}