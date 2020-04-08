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
        assertEquals(one(), EMPTY.add("foo", one()).get("foo"));
        assertEquals(Natural.atLeastOne(2),
                     EMPTY
                             .add("foo", one())
                             .add("foo", one())
                             .get("foo"));
        assertEquals(Natural.atLeastOne(11),
                     EMPTY.add("foo", one()).add("foo", Natural.atLeastOne(TEN)).get("foo"));
    }

    @Test
    public void addsOneByDefault() {
        assertEquals(EMPTY.add("foo", one()), EMPTY.add("foo"));
        assertEquals(EMPTY.add("foo", Natural.atLeastOne(2)), EMPTY.add("foo").add("foo"));
    }

    @Test
    public void remove() {
        assertEquals(EMPTY, EMPTY.remove("foo", one()));
        assertEquals(EMPTY, EMPTY.add("foo").remove("foo", one()));
        assertEquals(EMPTY, EMPTY.add("foo").remove("foo", Natural.atLeastOne(10)));
        assertEquals(EMPTY.add("foo", one()), EMPTY.add("foo", Natural.atLeastOne(2)).remove("foo", one()));
        assertEquals(EMPTY.add("foo", one()), EMPTY.add("foo", one()).remove("bar", one()));
    }

    @Test
    public void removesOneByDefault() {
        assertEquals(EMPTY, EMPTY.add("foo", one()).remove("foo"));
        assertEquals(EMPTY.add("foo", one()), EMPTY.add("foo", Natural.atLeastOne(2)).remove("foo"));
    }

    @Test
    public void contains() {
        assertFalse(EMPTY.contains("foo"));
        assertTrue(EMPTY.add("foo", one()).contains("foo"));
        assertFalse(EMPTY.add("foo", one()).contains("bar"));
    }

    @Test
    public void removeAll() {
        assertEquals(EMPTY, EMPTY.removeAll("foo"));
        assertEquals(EMPTY, EMPTY.add("foo", one()).removeAll("foo"));
        assertEquals(EMPTY, EMPTY.add("foo", Natural.atLeastOne(10)).removeAll("foo"));
        assertEquals(EMPTY.add("bar", one()), EMPTY.add("foo", Natural.atLeastOne(10)).add("bar", one()).removeAll("foo"));
    }

    @Test
    public void emptiness() {
        assertTrue(EMPTY.isEmpty());
        assertFalse(EMPTY.add("foo", one()).isEmpty());
        assertTrue(EMPTY.add("foo", one()).remove("foo", one()).isEmpty());
    }

    @Test
    public void head() {
        assertEquals(nothing(), EMPTY.head());
        assertEquals(just(tuple("foo", one())), EMPTY.add("foo", one()).head());
        assertEquals(just(tuple("foo", one())), EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)).head());
        assertEquals(just(tuple("bar", Natural.atLeastOne(10))),
                     EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)).remove("foo", one()).head());
    }

    @Test
    public void tail() {
        assertEquals(EMPTY, HashMultiSet.empty().tail());
        assertEquals(EMPTY, EMPTY.add("foo", one()).tail());
        assertEquals(EMPTY.add("bar", Natural.atLeastOne(10)), EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)).tail());
        assertEquals(EMPTY, EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)).remove("foo", one()).tail());
        assertEquals(EMPTY.add("bar", Natural.atLeastOne(10)),
                     EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)).add("foo", Natural.atLeastOne(9)).tail());
    }

    @Test
    public void iteration() {
        assertThat(EMPTY, isEmpty());
        assertThat(EMPTY.add("foo", one()), iterates(tuple("foo", one())));
        assertThat(EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)),
                   iterates(tuple("foo", one()), tuple("bar", Natural.atLeastOne(10))));
        assertThat(EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)).remove("foo", one()),
                   iterates(tuple("bar", Natural.atLeastOne(10))));
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(zero()), EMPTY.sizeInfo());
        assertEquals(known(one()), EMPTY.add("foo", one()).sizeInfo());
        assertEquals(known(abs(2)), EMPTY.add("foo", one()).add("foo", one()).sizeInfo());
        assertEquals(known(abs(12)), EMPTY.add("foo", one()).add("bar", Natural.atLeastOne(10)).add("foo", one()).sizeInfo());
    }

    @Test
    public void of() {
        assertEquals(EMPTY.add("a"), HashMultiSet.of("a"));
        assertEquals(EMPTY.add("a").add("b"), HashMultiSet.of("a", "b"));
        assertEquals(EMPTY.add("a", Natural.atLeastOne(2)).add("b"), HashMultiSet.of("a", "b", "a"));
    }

    @Test
    public void emptySingleton() {
        assertSame(HashMultiSet.empty(), HashMultiSet.empty());
    }

    @Test
    public void customEquivalenceRelationAndHashCode() {
        Integer saboteur = 666;
        HashMultiSet<Integer> identityHashMultiSet = HashMultiSet.<Integer>empty(referenceEquals(), identityHashCode())
                .add(1)
                .add(saboteur);
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
        assertEquals(EMPTY.add("foo", one()), EMPTY.add("foo", one()));
        assertNotEquals(EMPTY.add("foo", one()), EMPTY.add("bar", one()));
        assertEquals(EMPTY.add("foo", one()), EMPTY.add("foo", one()).add("bar", one()).remove("bar"));
        assertNotEquals(EMPTY, new Object());

        assertEquals(EMPTY.hashCode(), EMPTY.hashCode());
        assertEquals(EMPTY.add("foo", one()).hashCode(), EMPTY.add("foo", one()).hashCode());
        assertNotEquals(EMPTY.add("foo", one()).hashCode(), EMPTY.add("bar", one()).hashCode());
    }

    @Test
    public void toStringIsUseful() {
        assertEquals("HashMultiSet[(a * 1), (b * 2), (c * 3)]",
                     HashMultiSet.of("a", "b", "b", "c", "c", "c").toString());
        assertEquals("HashMultiSet[]", HashMultiSet.of("a").remove("a").toString());
    }
}