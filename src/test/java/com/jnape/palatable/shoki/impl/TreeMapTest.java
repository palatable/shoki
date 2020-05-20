package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.testsupport.traits.MapTraits;
import com.jnape.palatable.traitor.annotations.TestTraits;
import com.jnape.palatable.traitor.runners.Traits;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.Collections;
import java.util.Comparator;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static com.jnape.palatable.shoki.impl.TreeMap.treeMap;
import static com.jnape.palatable.shoki.impl.TreeSet.treeSet;
import static java.util.Comparator.comparing;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static testsupport.matchers.IterableMatcher.iterates;

@RunWith(Traits.class)
public class TreeMapTest {

    @TestTraits({MapTraits.class})
    public TreeMap<Object, Object> testSubject() {
        return treeMap(comparing(Object::hashCode));
    }

    @Test
    public void min() {
        assertEquals(nothing(), treeMap().min());
        assertEquals(just(tuple(1, "foo")),
                     treeMap(tuple(1, "foo"),
                             tuple(2, "bar"),
                             tuple(3, "baz")).min());
    }

    @Test
    public void max() {
        assertEquals(nothing(), treeMap().max());
        assertEquals(just(tuple(3, "baz")),
                     treeMap(tuple(1, "foo"),
                             tuple(2, "bar"),
                             tuple(3, "baz")).max());
    }

    @Test
    public void contains() {
        TreeMap<Integer, Object> treeMap = treeMap();
        assertFalse(treeMap.contains(1));
        assertFalse(treeMap.put(0, "foo").contains(1));
        assertTrue(treeMap.put(1, "foo").contains(1));
    }

    @Test
    public void sort() {
        assertEquals(treeMap(), treeMap().sort(naturalOrder()));

        assertEquals(treeMap(tuple(1, "foo"),
                             tuple(2, "bar"),
                             tuple(3, "baz")),
                     treeMap(tuple(1, "foo"),
                             tuple(2, "bar"),
                             tuple(3, "baz")).sort(naturalOrder()));

        assertEquals(treeMap(reverseOrder(),
                             tuple(3, "baz"),
                             tuple(2, "bar"),
                             tuple(1, "foo")),
                     treeMap(tuple(1, "foo"),
                             tuple(2, "bar"),
                             tuple(3, "baz"))
                             .sort(reverseOrder()));

        TreeMap<Integer, String> treeMap = treeMap(tuple(3, "baz"),
                                                   tuple(2, "bar"),
                                                   tuple(1, "foo"));
        assertSame(treeMap, treeMap.sort(naturalOrder()));
    }

    @Test
    public void reverse() {
        assertEquals(treeMap(reverseOrder()), treeMap().reverse());
        assertEquals(treeMap(reverseOrder(),
                             tuple(3, "baz"),
                             tuple(2, "bar"),
                             tuple(1, "foo")),
                     treeMap(tuple(1, "foo"),
                             tuple(2, "bar"),
                             tuple(3, "baz")).reverse());
    }

    @Test
    public void values() {
        assertEquals(strictQueue(), treeMap().values());
        assertEquals(strictQueue("foo", "bar"),
                     treeMap(tuple(1, "foo"),
                             tuple(2, "bar")).values());
        assertEquals(strictQueue("foo", "bar", "baz"),
                     treeMap(tuple(1, "foo"),
                             tuple(2, "bar"),
                             tuple(3, "baz")).values());
    }

    @Test
    public void sizeInfo() {
        assertEquals(known(zero()), treeMap().sizeInfo());
        assertEquals(known(one()), treeMap(tuple(1, "foo")).sizeInfo());
        assertEquals(known(abs(2)), treeMap(tuple(1, "foo"),
                                            tuple(2, "bar")).sizeInfo());
        assertEquals(known(one()), treeMap(tuple(1, "foo")).put(1, "bar").sizeInfo());
    }

    @Test
    public void equalsChecksComparatorAndUsesSameEntriesWithObjectEqualsForValues() {
        TreeMap<Integer, Integer> treeMap = treeMap();
        assertEquals(treeMap, treeMap);
        assertEquals(treeMap.put(1, 1), treeMap.put(1, 1));
        assertEquals(treeMap.put(1, 1).put(2, 2).remove(2), treeMap.put(1, 1));
        assertEquals(treeMap.put(1, 1).put(2, 2).put(3, 3).remove(3), treeMap.put(1, 1).put(2, 2));

        assertNotEquals(treeMap, treeMap.put(1, 1));
        assertNotEquals(treeMap, treeMap(reverseOrder()));
        assertNotEquals(treeMap.put(1, 1), treeMap.put(1, 2));
        assertNotEquals(treeMap.put(1, 1), treeMap.put(2, 1));
        assertNotEquals(treeMap(tuple(1, 2)), treeMap(tuple("foo", "bar")));
        assertNotEquals(treeMap, new Object());

        assertEquals(TreeMap.<Integer, Boolean>treeMap()
                             .put(0, true)
                             .put(32, false),
                     TreeMap.<Integer, Boolean>treeMap(Collections.reverseOrder(Comparator.<Integer>naturalOrder()))
                             .put(0, true)
                             .put(32, false)
                             .put(64, true)
                             .reverse()
                             .remove(64));
    }

    @Test
    public void hashCodeUsesAndObjectHashForKeysAndValuesForEqualsSymmetry() {
        TreeMap<Integer, Integer> treeMap = treeMap();
        assertEquals(treeMap.hashCode(), treeMap.hashCode());
        assertEquals(treeMap.put(1, 1).hashCode(), treeMap.put(1, 1).hashCode());

        assertNotEquals(treeMap.hashCode(), treeMap.put(0, 1).hashCode());
        assertNotEquals(treeMap.put(1, 1).hashCode(), treeMap.put(1, 2).hashCode());
        assertNotEquals(treeMap().hashCode(), treeMap(reverseOrder()).hashCode());

        assertEquals(treeMap.put(0, 1).put(32, 2).remove(32).hashCode(), treeMap.put(0, 1).hashCode());
    }

    @Test
    public void usefulToString() {
        assertEquals("TreeMap[]", treeMap().toString());
        assertEquals("TreeMap[(a=1), (b=2), (c=3), (d=4), (e=5), (f=6)]",
                     treeMap(tuple("a", 1),
                             tuple("b", 2),
                             tuple("c", 3),
                             tuple("d", 4),
                             tuple("e", 5),
                             tuple("f", 6)).toString());
    }

    @Test
    public void keys() {
        assertEquals(treeSet(), treeMap().keys());
        assertEquals(treeSet(1, 2, 3), treeMap(tuple(1, "foo"),
                                               tuple(2, "bar"),
                                               tuple(3, "baz")).keys());
        assertEquals(treeSet(reverseOrder(), 1, 2, 3), treeMap(reverseOrder(),
                                                               tuple(1, "foo"),
                                                               tuple(2, "bar"),
                                                               tuple(3, "baz")).keys());
    }

    @Test
    public void iteration() {
        assertThat(treeMap(tuple("a", 1),
                           tuple("b", 2),
                           tuple("c", 3),
                           tuple("d", 4),
                           tuple("e", 5),
                           tuple("f", 6)),
                   iterates(tuple("a", 1),
                            tuple("b", 2),
                            tuple("c", 3),
                            tuple("d", 4),
                            tuple("e", 5),
                            tuple("f", 6)));
    }
}
