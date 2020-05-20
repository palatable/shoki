package com.jnape.palatable.shoki.interop;

import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;
import com.jnape.palatable.shoki.impl.TreeMap;
import com.jnape.palatable.shoki.impl.TreeMultiSet;
import com.jnape.palatable.shoki.impl.TreeSet;
import org.junit.Test;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn2.ToMap.toMap;
import static com.jnape.palatable.shoki.api.Natural.atLeastOne;
import static com.jnape.palatable.shoki.api.Natural.one;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;
import static org.junit.Assert.assertEquals;

public class ShokiTest {

    @Test
    public void strictStack() {
        assertEquals(StrictStack.strictStack(),
                     Shoki.strictStack(Collections::emptyIterator));
        assertEquals(StrictStack.strictStack(1, 2, 3, 4, 5),
                     Shoki.strictStack(new ArrayDeque<>(asList(1, 2, 3, 4, 5))));
        assertEquals(StrictStack.strictStack(1, 2, 3, 4, 5),
                     Shoki.strictStack(asList(1, 2, 3, 4, 5)));
        assertEquals(StrictStack.strictStack(1, 2, 3, 4, 5),
                     Shoki.strictStack(new LinkedHashSet<>(asList(1, 2, 3, 4, 5))));
    }

    @Test
    public void strictQueue() {
        assertEquals(StrictQueue.strictQueue(), Shoki.strictQueue(emptyList()));

        Collection<Integer> javaCollection = asList(1, 2, 3, 4, 5);
        assertEquals(StrictQueue.strictQueue(1, 2, 3, 4, 5), Shoki.strictQueue(javaCollection));
    }

    @Test
    public void hashMap() {
        assertEquals(HashMap.hashMap(), Shoki.hashMap(emptyMap()));

        java.util.Map<String, Integer> javaMap = toMap(java.util.HashMap::new,
                                                       asList(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)));
        assertEquals(HashMap.hashMap(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)), Shoki.hashMap(javaMap));
    }

    @Test
    public void hashSet() {
        assertEquals(HashSet.hashSet(), Shoki.hashSet(emptyList()));
        List<Integer> javaCollection = asList(1, 2, 3);
        assertEquals(HashSet.hashSet(1, 2, 3), Shoki.hashSet(javaCollection));
    }

    @Test
    public void hashMultiSet() {
        assertEquals(HashMultiSet.hashMultiSet(), Shoki.hashMultiSet(emptyMap()));
        assertEquals(HashMultiSet.hashMultiSet(), Shoki.hashMultiSet(emptyList()));

        List<Integer> javaCollection = asList(1, 2, 2, 3, 3, 3);
        assertEquals(HashMultiSet.hashMultiSet(1, 2, 2, 3, 3, 3), Shoki.hashMultiSet(javaCollection));

        Map<String, Integer> javaMap = toMap(java.util.HashMap::new,
                                             asList(tuple("foo", 1), tuple("bar", -2),
                                                    tuple("baz", 3), tuple("quux", 0)));
        assertEquals(HashMultiSet.<String>hashMultiSet()
                             .inc("foo", one())
                             .inc("baz", atLeastOne(3)),
                     Shoki.hashMultiSet(javaMap));
    }

    @Test
    public void treeMap() {
        assertEquals(TreeMap.treeMap(), Shoki.<Integer, Integer>treeMap(emptyMap()));

        java.util.Map<String, Integer> javaHashMap = toMap(java.util.HashMap::new,
                                                           asList(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)));
        assertEquals(TreeMap.treeMap(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)),
                     Shoki.treeMap(javaHashMap));

        java.util.Map<String, Integer> javaTreeMapWithoutComparator =
                toMap(java.util.TreeMap::new, asList(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)));
        assertEquals(TreeMap.treeMap(naturalOrder(), tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)),
                     Shoki.treeMap(javaTreeMapWithoutComparator));

        java.util.Map<String, Integer> javaTreeMapWithComparator =
                toMap(() -> new java.util.TreeMap<>(reverseOrder()),
                      asList(tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)));
        assertEquals(TreeMap.treeMap(reverseOrder(), tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)),
                     Shoki.treeMap(javaTreeMapWithComparator));

        assertEquals(TreeMap.treeMap(naturalOrder(), tuple("foo", 1), tuple("bar", 2), tuple("baz", 3)),
                     Shoki.treeMap(naturalOrder(), javaTreeMapWithComparator));
    }

    @Test
    public void treeSet() {
        assertEquals(TreeSet.treeSet(), Shoki.<Integer>treeSet(emptyList()));
        List<Integer> javaCollection = asList(1, 2, 3);
        assertEquals(TreeSet.treeSet(1, 2, 3), Shoki.treeSet(javaCollection));

        java.util.TreeSet<Integer> javaTreeSetWithoutComparator = new java.util.TreeSet<>(asList(1, 2, 3));
        assertEquals(TreeSet.treeSet(naturalOrder(), 1, 2, 3), Shoki.treeSet(javaTreeSetWithoutComparator));

        java.util.TreeSet<Integer> javaTreeSetWithComparator = new java.util.TreeSet<Integer>(reverseOrder()) {{
            addAll(asList(1, 2, 3));
        }};
        assertEquals(TreeSet.treeSet(reverseOrder(), 1, 2, 3), Shoki.treeSet(javaTreeSetWithComparator));

        assertEquals(TreeSet.treeSet(naturalOrder(), 1, 2, 3),
                     Shoki.treeSet(naturalOrder(), javaTreeSetWithComparator));
    }

    @Test
    public void treeMultiSet() {
        assertEquals(TreeMultiSet.treeMultiSet(), Shoki.<Integer>treeMultiSet(emptyMap()));
        assertEquals(TreeMultiSet.treeMultiSet(), Shoki.<Integer>treeMultiSet(emptyList()));

        Map<String, Integer> javaMapWithoutComparable = toMap(java.util.HashMap::new,
                                                              asList(tuple("foo", 1), tuple("bar", -2),
                                                                     tuple("baz", 3), tuple("quux", 0)));
        assertEquals(TreeMultiSet.<String>treeMultiSet()
                             .inc("foo", one())
                             .inc("baz", atLeastOne(3)),
                     Shoki.treeMultiSet(javaMapWithoutComparable));

        Map<String, Integer> javaMapWithComparable = toMap(() -> new java.util.TreeMap<>(reverseOrder()),
                                                           asList(tuple("foo", 1), tuple("bar", -2),
                                                                  tuple("baz", 3), tuple("quux", 0)));

        assertEquals(TreeMultiSet.<String>treeMultiSet(reverseOrder())
                             .inc("foo", one())
                             .inc("baz", atLeastOne(3)),
                     Shoki.treeMultiSet(javaMapWithComparable));

        assertEquals(TreeMultiSet.<String>treeMultiSet(naturalOrder())
                             .inc("foo", one())
                             .inc("baz", atLeastOne(3)),
                     Shoki.treeMultiSet(naturalOrder(), javaMapWithComparable));

        Iterable<Integer> javaIterableWithoutComparable = asList(1, 2, 2, 3, 3, 3);
        assertEquals(TreeMultiSet.treeMultiSet(naturalOrder(), 1, 2, 2, 3, 3, 3),
                     Shoki.treeMultiSet(javaIterableWithoutComparable));

        Iterable<Integer> javaIterableWithComparable = new java.util.TreeSet<Integer>(reverseOrder()) {{
            addAll(asList(1, 2, 3));
        }};
        assertEquals(TreeMultiSet.treeMultiSet(reverseOrder(), 1, 2, 3),
                     Shoki.treeMultiSet(javaIterableWithComparable));

        assertEquals(TreeMultiSet.treeMultiSet(naturalOrder(), 1, 2, 3),
                     Shoki.treeMultiSet(naturalOrder(), javaIterableWithComparable));
    }
}