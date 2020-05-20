package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.testsupport.DefaultMethodsSet;
import org.junit.Test;
import testsupport.matchers.IterableMatcher;

import java.util.Comparator;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.TreeSet.treeSet;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static java.util.Collections.reverseOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.iterates;

public class TreeSetTest {

    @Test
    public void add() {
        assertEquals(treeSet("foo"), TreeSet.<String>treeSet().add("foo"));
        assertEquals(treeSet("foo"), TreeSet.<String>treeSet().add("foo").add("foo"));
        assertEquals(treeSet("foo", "bar"), TreeSet.<String>treeSet().add("foo").add("bar"));
    }

    @Test
    public void remove() {
        assertEquals(TreeSet.<String>treeSet(), TreeSet.<String>treeSet().remove("foo"));
        assertEquals(TreeSet.<String>treeSet(), TreeSet.<String>treeSet().add("foo").remove("foo"));
        assertEquals(treeSet("bar"), TreeSet.<String>treeSet().add("foo").add("bar").remove("foo"));
    }

    @Test
    public void head() {
        assertEquals(nothing(), treeSet().head());
        assertEquals(just("foo"), TreeSet.<String>treeSet().add("foo").head());
        assertEquals(just("bar"), TreeSet.<String>treeSet().add("foo").add("bar").head());
        assertEquals(just("bar"), TreeSet.<String>treeSet().add("bar").add("foo").head());
    }

    @Test
    public void tail() {
        assertEquals(treeSet(), treeSet().tail());
        assertEquals(treeSet(), TreeSet.<String>treeSet().add("foo").tail());
        assertEquals(treeSet("foo"), TreeSet.<String>treeSet().add("foo").add("bar").tail());
        assertEquals(treeSet("foo"), TreeSet.<String>treeSet().add("bar").add("foo").tail());
    }

    @Test
    public void contains() {
        assertFalse(TreeSet.<String>treeSet().contains("foo"));
        assertFalse(TreeSet.<String>treeSet().add("bar").contains("foo"));
        assertTrue(TreeSet.<String>treeSet().add("foo").contains("foo"));
        assertTrue(TreeSet.<String>treeSet().add("foo").add("bar").contains("foo"));
        assertFalse(TreeSet.<String>treeSet().add("foo").remove("foo").contains("foo"));
    }

    @Test
    public void isEmpty() {
        assertTrue(treeSet().isEmpty());
        assertFalse(TreeSet.<String>treeSet().add("foo").isEmpty());
        assertTrue(TreeSet.<String>treeSet().add("foo").remove("foo").isEmpty());
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(zero()), treeSet().sizeInfo());
        assertEquals(known(one()), TreeSet.<String>treeSet().add("foo").sizeInfo());
        assertEquals(known(one()), TreeSet.<String>treeSet().add("foo").add("foo").sizeInfo());
        assertEquals(known(abs(2)), TreeSet.<String>treeSet().add("foo").add("bar").sizeInfo());
        assertEquals(known(zero()), TreeSet.<String>treeSet().add("foo").remove("foo").sizeInfo());
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(treeSet(), treeSet());
        assertNotEquals(treeSet(), TreeSet.<String>treeSet().add("foo"));
        assertEquals(TreeSet.<String>treeSet(), TreeSet.<String>treeSet().add("foo").remove("foo"));
        assertEquals(TreeSet.<String>treeSet().add("foo"), TreeSet.<String>treeSet().add("foo"));
        assertNotEquals(TreeSet.<String>treeSet().add("foo"), TreeSet.<String>treeSet().add("bar"));
        assertNotEquals(TreeSet.<String>treeSet().add("foo"), new Object());

        assertNotEquals(treeSet(), treeSet(reverseOrder()));

        assertEquals(treeSet().hashCode(), treeSet().hashCode());
        assertNotEquals(treeSet().hashCode(), treeSet("foo").hashCode());
        assertNotEquals(treeSet("foo").hashCode(), treeSet("bar").hashCode());
        assertEquals(treeSet("foo").hashCode(), treeSet("foo", "foo").hashCode());
    }

    @Test
    public void usefulToString() {
        assertEquals("TreeSet[]", treeSet().toString());
        assertEquals("TreeSet[bar, baz, foo]", TreeSet.<String>treeSet().add("foo").add("bar").add("baz").toString());
        assertEquals("TreeSet[bar, baz, foo]", TreeSet.<String>treeSet().add("bar").add("baz").add("foo").toString());
    }

    @Test
    public void customComparator() {
        TreeSet<Integer> reverseTreeSet = TreeSet.<Integer>treeSet(Comparator.reverseOrder())
                .add(1)
                .add(2)
                .add(3);
        assertEquals(just(3), reverseTreeSet.head());
        assertEquals(just(2), reverseTreeSet.tail().head());
        assertEquals(just(1), reverseTreeSet.tail().tail().head());
    }

    @Test
    public void intersection() {
        assertEquals(treeSet(), treeSet().intersection(treeSet()));
        assertEquals(treeSet(), TreeSet.<Integer>treeSet().intersection(treeSet(1, 2, 3)));
        assertEquals(treeSet(), treeSet(1, 2, 3).intersection(treeSet()));
        assertEquals(treeSet(1, 2, 3), treeSet(1, 2, 3).intersection(treeSet(1, 2, 3)));
        assertEquals(treeSet(2, 3), treeSet(1, 2, 3).intersection(treeSet(2, 3, 4)));
        assertEquals(treeSet(2, 3), treeSet(2, 3, 4).intersection(treeSet(1, 2, 3)));
    }

    @Test
    public void union() {
        assertEquals(treeSet(), treeSet().union(treeSet()));
        assertEquals(treeSet(1, 2, 3), TreeSet.<Integer>treeSet().union(treeSet(1, 2, 3)));
        assertEquals(treeSet(1, 2, 3), treeSet(1, 2, 3).union(treeSet()));
        assertEquals(treeSet(1, 2, 3), treeSet(1, 2, 3).union(treeSet(1, 2, 3)));
        assertEquals(treeSet(1, 2, 3, 4), treeSet(1, 2, 3).union(treeSet(2, 3, 4)));
        assertEquals(treeSet(1, 2, 3, 4), treeSet(2, 3, 4).union(treeSet(1, 2, 3)));
    }

    @Test
    public void difference() {
        assertEquals(treeSet(), treeSet().difference(treeSet()));
        assertEquals(treeSet(), TreeSet.<Integer>treeSet().difference(treeSet(1, 2, 3)));
        assertEquals(treeSet(1, 2, 3), treeSet(1, 2, 3).difference(treeSet()));
        assertEquals(treeSet(), treeSet(1, 2, 3).difference(treeSet(1, 2, 3)));
        assertEquals(treeSet(1), treeSet(1, 2, 3).difference(treeSet(2, 3, 4)));
        assertEquals(treeSet(4), treeSet(2, 3, 4).difference(treeSet(1, 2, 3)));
    }

    @Test
    public void symmetricDifference() {
        TreeSet<Integer> empty = treeSet();
        assertThat(DefaultMethodsSet.delegate(empty).symmetricDifference(empty),
                   equivalentTo(empty.symmetricDifference(empty), sameElements()));
    }

    @Test
    public void min() {
        assertEquals(nothing(), treeSet().min());
        assertEquals(just(1), treeSet(1).min());
        assertEquals(just(1), treeSet(1, 2).min());
        assertEquals(just(1), treeSet(2, 1).min());
    }

    @Test
    public void max() {
        assertEquals(nothing(), treeSet().max());
        assertEquals(just(2), treeSet(2).max());
        assertEquals(just(2), treeSet(1, 2).max());
        assertEquals(just(2), treeSet(2, 1).max());
    }

    @Test
    public void sort() {
        assertEquals(treeSet(reverseOrder()), treeSet().sort(reverseOrder()));
        assertEquals(treeSet(reverseOrder(), 1, 2, 3), treeSet(1, 2, 3).sort(reverseOrder()));
    }

    @Test
    public void reverse() {
        assertEquals(treeSet(reverseOrder()), treeSet().reverse());
        assertEquals(treeSet(reverseOrder(), 3, 2, 1), treeSet(1, 2, 3).reverse());
        assertEquals(treeSet(), treeSet().reverse().reverse());
    }

    @Test
    public void iteration() {
        assertThat(TreeSet.<Integer>treeSet(), IterableMatcher.isEmpty());
        assertThat(treeSet(1, 2, 3), iterates(1, 2, 3));
        assertThat(treeSet(reverseOrder(), 1, 2, 3), iterates(3, 2, 1));
    }
}