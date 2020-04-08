package com.jnape.palatable.shoki.api;

import org.junit.Test;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static java.util.Comparator.comparing;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class EquivalenceRelationTest {

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void objectEquals() {
        EquivalenceRelation<Object> objectEquals = EquivalenceRelation.objectEquals();
        assertThat(1, equivalentTo(1, objectEquals));
        assertThat(1, equivalentTo(new Integer(1), objectEquals));
        assertThat(1, not(equivalentTo("2", objectEquals)));
        assertThat(new Object(), not(equivalentTo(new Object(), objectEquals)));
    }

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void referenceEquals() {
        EquivalenceRelation<Object> referenceEquals = EquivalenceRelation.referenceEquals();
        assertThat(1, equivalentTo(1, referenceEquals));
        assertThat(1, not(equivalentTo(new Integer(1), referenceEquals)));
        assertThat(1, not(equivalentTo("2", referenceEquals)));
        assertThat(new Object(), not(equivalentTo(new Object(), referenceEquals)));
    }

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void comparablyEqualsWithComparator() {
        EquivalenceRelation<Object> comparablyEquals =
                EquivalenceRelation.comparablyEquals(comparing(Object::hashCode));
        assertThat(1, equivalentTo(1, comparablyEquals));
        assertThat(1, equivalentTo(new Integer(1), comparablyEquals));
        assertThat(1, not(equivalentTo("2", comparablyEquals)));
        assertThat(new Object(), not(equivalentTo(new Object(), comparablyEquals)));
    }

    @Test
    @SuppressWarnings("UnnecessaryBoxing")
    public void comparablyEquals() {
        EquivalenceRelation<Integer> comparablyEquals = EquivalenceRelation.comparablyEquals();
        assertThat(1, equivalentTo(1, comparablyEquals));
        assertThat(1, equivalentTo(new Integer(1), comparablyEquals));
        assertThat(1, not(equivalentTo(2, comparablyEquals)));
    }

    @Test
    public void equivalence() {
        assertTrue(equivalent(1, 1, EquivalenceRelation.objectEquals()));
        assertFalse(equivalent(1, 2, EquivalenceRelation.objectEquals()));
    }
}