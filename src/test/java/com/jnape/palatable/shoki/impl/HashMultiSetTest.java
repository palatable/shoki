package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.api.Natural;
import org.junit.Test;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.referenceEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.identityHashCode;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static java.math.BigInteger.TEN;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.isEmpty;
import static testsupport.matchers.IterableMatcher.iterates;

public class HashMultiSetTest {

    private static final HashMultiSet<String> EMPTY = HashMultiSet.empty();

    @Test
    public void add() {
        assertEquals(zero(), EMPTY.get("foo"));
        assertEquals(one(), EMPTY.inc("foo", one()).get("foo"));
        assertEquals(Natural.atLeastOne(2),
                     EMPTY
                             .inc("foo", one())
                             .inc("foo", one())
                             .get("foo"));
        assertEquals(Natural.atLeastOne(11),
                     EMPTY.inc("foo", one()).inc("foo", Natural.atLeastOne(TEN)).get("foo"));
    }

    @Test
    public void addsOneByDefault() {
        assertEquals(EMPTY.inc("foo", one()), EMPTY.inc("foo"));
        assertEquals(EMPTY.inc("foo", Natural.atLeastOne(2)), EMPTY.inc("foo").inc("foo"));
    }

    @Test
    public void remove() {
        assertEquals(EMPTY, EMPTY.dec("foo", one()));
        assertEquals(EMPTY, EMPTY.inc("foo").dec("foo", one()));
        assertEquals(EMPTY, EMPTY.inc("foo").dec("foo", Natural.atLeastOne(10)));
        assertEquals(EMPTY.inc("foo", one()), EMPTY.inc("foo", Natural.atLeastOne(2)).dec("foo", one()));
        assertEquals(EMPTY.inc("foo", one()), EMPTY.inc("foo", one()).dec("bar", one()));
    }

    @Test
    public void removesOneByDefault() {
        assertEquals(EMPTY, EMPTY.inc("foo", one()).dec("foo"));
        assertEquals(EMPTY.inc("foo", one()), EMPTY.inc("foo", Natural.atLeastOne(2)).dec("foo"));
    }

    @Test
    public void contains() {
        assertFalse(EMPTY.contains("foo"));
        assertTrue(EMPTY.inc("foo", one()).contains("foo"));
        assertFalse(EMPTY.inc("foo", one()).contains("bar"));
    }

    @Test
    public void removeAll() {
        assertEquals(EMPTY, EMPTY.remove("foo"));
        assertEquals(EMPTY, EMPTY.inc("foo", one()).remove("foo"));
        assertEquals(EMPTY, EMPTY.inc("foo", Natural.atLeastOne(10)).remove("foo"));
        assertEquals(EMPTY.inc("bar", one()), EMPTY.inc("foo", Natural.atLeastOne(10)).inc("bar", one()).remove("foo"));
    }

    @Test
    public void emptiness() {
        assertTrue(EMPTY.isEmpty());
        assertFalse(EMPTY.inc("foo", one()).isEmpty());
        assertTrue(EMPTY.inc("foo", one()).dec("foo", one()).isEmpty());
    }

    @Test
    public void head() {
        assertEquals(nothing(), EMPTY.head());
        assertEquals(just(tuple("foo", one())), EMPTY.inc("foo", one()).head());
        assertEquals(just(tuple("foo", one())), EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)).head());
        assertEquals(just(tuple("bar", Natural.atLeastOne(10))),
                     EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)).dec("foo", one()).head());
    }

    @Test
    public void tail() {
        assertEquals(EMPTY, HashMultiSet.empty().tail());
        assertEquals(EMPTY, EMPTY.inc("foo", one()).tail());
        assertEquals(EMPTY.inc("bar", Natural.atLeastOne(10)), EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)).tail());
        assertEquals(EMPTY, EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)).dec("foo", one()).tail());
        assertEquals(EMPTY.inc("bar", Natural.atLeastOne(10)),
                     EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)).inc("foo", Natural.atLeastOne(9)).tail());
    }

    @Test
    public void iteration() {
        assertThat(EMPTY, isEmpty());
        assertThat(EMPTY.inc("foo", one()), iterates(tuple("foo", one())));
        assertThat(EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)),
                   iterates(tuple("foo", one()), tuple("bar", Natural.atLeastOne(10))));
        assertThat(EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)).dec("foo", one()),
                   iterates(tuple("bar", Natural.atLeastOne(10))));
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(zero()), EMPTY.sizeInfo());
        assertEquals(known(one()), EMPTY.inc("foo", one()).sizeInfo());
        assertEquals(known(abs(2)), EMPTY.inc("foo", one()).inc("foo", one()).sizeInfo());
        assertEquals(known(abs(12)), EMPTY.inc("foo", one()).inc("bar", Natural.atLeastOne(10)).inc("foo", one()).sizeInfo());
    }

    @Test
    public void of() {
        assertEquals(EMPTY.inc("a"), HashMultiSet.of("a"));
        assertEquals(EMPTY.inc("a").inc("b"), HashMultiSet.of("a", "b"));
        assertEquals(EMPTY.inc("a", Natural.atLeastOne(2)).inc("b"), HashMultiSet.of("a", "b", "a"));
    }

    @Test
    public void emptySingleton() {
        assertSame(HashMultiSet.empty(), HashMultiSet.empty());
    }

    @Test
    public void customEquivalenceRelationAndHashCode() {
        Integer saboteur = 666;
        HashMultiSet<Integer> identityHashMultiSet = HashMultiSet.<Integer>empty(referenceEquals(), identityHashCode())
                .inc(1)
                .inc(saboteur);
        assertTrue(identityHashMultiSet.contains(1));
        assertFalse(identityHashMultiSet.contains(666));
        assertTrue(identityHashMultiSet.contains(saboteur));

        assertEquals(identityHashMultiSet, HashMultiSet.of(referenceEquals(), identityHashCode(), 1, saboteur));
        assertNotEquals(HashMultiSet.of(1, 666), identityHashMultiSet);
        assertEquals(identityHashMultiSet, HashMultiSet.of(1, 666));
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(EMPTY, EMPTY);
        assertEquals(EMPTY.inc("foo", one()), EMPTY.inc("foo", one()));
        assertNotEquals(EMPTY.inc("foo", one()), EMPTY.inc("bar", one()));
        assertEquals(EMPTY.inc("foo", one()), EMPTY.inc("foo", one()).inc("bar", one()).dec("bar"));
        assertNotEquals(EMPTY, new Object());

        assertEquals(EMPTY.hashCode(), EMPTY.hashCode());
        assertEquals(EMPTY.inc("foo", one()).hashCode(), EMPTY.inc("foo", one()).hashCode());
        assertNotEquals(EMPTY.inc("foo", one()).hashCode(), EMPTY.inc("bar", one()).hashCode());
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("HashMultiSet[(a * 1), (b * 2), (c * 3)]",
                     HashMultiSet.of("a", "b", "b", "c", "c", "c").toString());
        assertEquals("HashMultiSet[]", HashMultiSet.of("a").dec("a").toString());
    }
}