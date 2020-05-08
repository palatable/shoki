package com.jnape.palatable.shoki.interop;

import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;
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
}