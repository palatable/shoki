package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsMap;
import org.junit.Test;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.impl.HashMap.empty;
import static org.junit.Assert.assertTrue;

public class MapTest {

    @Test
    public void putAllWithSemigroup() {
        Semigroup<Integer>                          sum   = Integer::sum;
        DefaultMethodsMap<Integer, String, Integer> empty = DefaultMethodsMap.delegate(empty());

        assertTrue(Map.equals(empty.merge(empty, sum), empty, objectEquals()));
        assertTrue(Map.equals(empty.merge(HashMap.of(tuple("foo", 1)), sum),
                              HashMap.of(tuple("foo", 1)),
                              objectEquals()));
        assertTrue(Map.equals(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1))).merge(empty, sum),
                              HashMap.of(tuple("foo", 1)),
                              objectEquals()));
        assertTrue(Map.equals(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1), tuple("bar", 2)))
                                      .merge(HashMap.of(tuple("foo", 1), tuple("baz", 3)), sum),
                              HashMap.of(tuple("foo", 2), tuple("bar", 2), tuple("baz", 3)),
                              objectEquals()));
    }

    @Test
    public void removeAll() {
        DefaultMethodsMap<Integer, String, Integer> empty = DefaultMethodsMap.delegate(empty());

        assertTrue(Map.equals(empty.removeAll(HashSet.empty()), empty, objectEquals()));
        assertTrue(Map.equals(empty.removeAll(HashSet.of("foo", "bar")), empty, objectEquals()));
        assertTrue(Map.equals(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1))).removeAll(HashSet.empty()),
                              HashMap.of(tuple("foo", 1)),
                              objectEquals()));
        assertTrue(Map.equals(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1), tuple("bar", 2)))
                                      .removeAll(HashSet.of("foo", "baz")),
                              HashMap.of(tuple("bar", 2)),
                              objectEquals()));
    }
}