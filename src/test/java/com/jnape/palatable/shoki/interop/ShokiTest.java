package com.jnape.palatable.shoki.interop;

import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;
import org.junit.Test;

import java.util.AbstractList;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn2.ToMap.toMap;
import static com.jnape.palatable.shoki.api.Natural.atLeastOne;
import static com.jnape.palatable.shoki.api.Natural.one;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ShokiTest {

    @Test
    public void strictStack() {
        assertEquals(StrictStack.strictStack(), Shoki.strictStack(emptyList()));

        StrictStack<Integer> stackOneThroughFive = StrictStack.strictStack(1, 2, 3, 4, 5);
        List<Integer>        listOneThroughFive  = asList(1, 2, 3, 4, 5);

        AtomicBoolean descendingIteratorUsed = new AtomicBoolean();
        Deque<Integer> deque = new ArrayDeque<Integer>(listOneThroughFive) {
            @Override
            public Iterator<Integer> descendingIterator() {
                descendingIteratorUsed.set(true);
                return super.descendingIterator();
            }
        };
        assertEquals(stackOneThroughFive, Shoki.strictStack(deque));
        assertTrue(descendingIteratorUsed.get());

        AtomicInteger randomAccessCount = new AtomicInteger();
        ArrayList<Integer> randomAccessList = new ArrayList<Integer>(listOneThroughFive) {
            @Override
            public Integer get(int index) {
                randomAccessCount.incrementAndGet();
                return super.get(index);
            }
        };
        assertEquals(stackOneThroughFive, Shoki.strictStack(randomAccessList));
        assertEquals(5, randomAccessCount.get());

        AtomicBoolean listIteratorUsed = new AtomicBoolean();
        List<Integer> sequentialAccessList = new AbstractList<Integer>() {
            private final List<Integer> delegate = new ArrayList<>(listOneThroughFive);

            @Override
            public Integer get(int index) {
                return delegate.get(index);
            }

            @Override
            public int size() {
                return delegate.size();
            }

            @Override
            public ListIterator<Integer> listIterator(int index) {
                listIteratorUsed.set(true);
                return super.listIterator(index);
            }
        };
        assertEquals(stackOneThroughFive, Shoki.strictStack(sequentialAccessList));
        assertTrue(listIteratorUsed.get());

        Collection<Integer> collection = new LinkedHashSet<>(listOneThroughFive);
        assertEquals(stackOneThroughFive, Shoki.strictStack(collection));
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
}