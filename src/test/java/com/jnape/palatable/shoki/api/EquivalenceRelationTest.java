package com.jnape.palatable.shoki.api;

import org.junit.Test;

import static java.util.Comparator.comparing;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class EquivalenceRelationTest {

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void objectEquals() {
        EquivalenceRelation<Object> objectEquals = EquivalenceRelation.objectEquals();
        assertTrue(objectEquals.apply(1, 1));
        assertTrue(objectEquals.apply(1, new Integer(1)));
        assertFalse(objectEquals.apply(1, "2"));
        assertFalse(objectEquals.apply(new Object(), new Object()));
    }

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void referenceEquals() {
        EquivalenceRelation<Object> referenceEquals = EquivalenceRelation.referenceEquals();
        assertTrue(referenceEquals.apply(1, 1));
        assertFalse(referenceEquals.apply(1, new Integer(1)));
        assertFalse(referenceEquals.apply(1, "2"));
        assertFalse(referenceEquals.apply(new Object(), new Object()));
    }

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void comparablyEqualsWithComparator() {
        EquivalenceRelation<Object> comparablyEquals =
                EquivalenceRelation.comparablyEquals(comparing(Object::hashCode));
        assertTrue(comparablyEquals.apply(1, 1));
        assertTrue(comparablyEquals.apply(1, new Integer(1)));
        assertFalse(comparablyEquals.apply(1, "2"));
        assertFalse(comparablyEquals.apply(new Object(), new Object()));
    }

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void comparablyEquals() {
        EquivalenceRelation<Integer> comparablyEquals =
                EquivalenceRelation.comparablyEquals();
        assertTrue(comparablyEquals.apply(1, 1));
        assertTrue(comparablyEquals.apply(1, new Integer(1)));
        assertFalse(comparablyEquals.apply(1, 2));
    }
}