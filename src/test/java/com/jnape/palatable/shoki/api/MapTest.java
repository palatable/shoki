package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsMap;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Map.EquivalenceRelations.entries;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.impl.HashMap.hashMap;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static com.jnape.palatable.shoki.testsupport.HashingAlgorithmMatcher.hashesEquivalentlyTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@RunWith(Enclosed.class)
public class MapTest {

    public static final class DefaultMethods {

        @Test
        public void contains() {
            DefaultMethodsMap<Natural, String, Integer> empty = DefaultMethodsMap.delegate(hashMap());

            assertFalse(empty.contains("foo"));
            assertTrue(empty.put("foo", 1).contains("foo"));
            assertFalse(empty.put("foo", 1).contains("bar"));
        }

        @Test
        public void putAllWithSemigroup() {
            EquivalenceRelation<Map<Natural, String, Integer>> sameEntries = entries(objectEquals());

            Semigroup<Integer>                          sum   = Integer::sum;
            DefaultMethodsMap<Natural, String, Integer> empty = DefaultMethodsMap.delegate(hashMap());

            assertThat(empty.merge(empty, sum),
                       equivalentTo(empty, sameEntries));
            assertThat(empty.merge(hashMap(tuple("foo", 1)), sum),
                       equivalentTo(hashMap(tuple("foo", 1)), sameEntries));
            assertThat(DefaultMethodsMap.delegate(hashMap(tuple("foo", 1))).merge(empty, sum),
                       equivalentTo(hashMap(tuple("foo", 1)), sameEntries));
            assertThat(DefaultMethodsMap.delegate(hashMap(tuple("foo", 1), tuple("bar", 2)))
                               .merge(hashMap(tuple("foo", 1), tuple("baz", 3)), sum),
                       equivalentTo(hashMap(tuple("foo", 2), tuple("bar", 2), tuple("baz", 3)), sameEntries));
        }


        @Test
        public void removeAll() {
            EquivalenceRelation<Map<Natural, String, Integer>> sameEntries = entries(objectEquals());
            DefaultMethodsMap<Natural, String, Integer>        empty       = DefaultMethodsMap.delegate(hashMap());

            assertThat(empty.removeAll(hashSet()),
                       equivalentTo(empty, sameEntries));
            assertThat(empty.removeAll(hashSet("foo", "bar")),
                       equivalentTo(empty, sameEntries));
            assertThat(DefaultMethodsMap.delegate(hashMap(tuple("foo", 1))).removeAll(hashSet()),
                       equivalentTo(hashMap(tuple("foo", 1)), sameEntries));
            assertThat(DefaultMethodsMap.delegate(hashMap(tuple("foo", 1), tuple("bar", 2)))
                               .removeAll(hashSet("foo", "baz")),
                       equivalentTo(hashMap(tuple("bar", 2)), sameEntries));
        }
    }

    public static final class EquivalenceRelations {

        @Test
        public void sameEntries() {
            EquivalenceRelation<HashMap<String, Integer>> sameEntries =
                    Map.EquivalenceRelations.entries(objectEquals());

            assertThat(hashMap(), equivalentTo(hashMap(), sameEntries));
            assertThat(hashMap(tuple("foo", 1), tuple("bar", 2)),
                       equivalentTo(hashMap(tuple("foo", 1), tuple("bar", 2)), sameEntries));
            assertThat(hashMap(),
                       not(equivalentTo(hashMap(tuple("foo", 1), tuple("bar", 2)), sameEntries)));
        }

        @Test
        public void sameKeys() {
            EquivalenceRelation<HashMap<String, Integer>> sameKeys =
                    Map.EquivalenceRelations.keys(sameElements());

            assertThat(hashMap(), equivalentTo(hashMap(), sameKeys));
            assertThat(hashMap(tuple("foo", 1), tuple("bar", 2)),
                       equivalentTo(hashMap(tuple("foo", 2), tuple("bar", 1)), sameKeys));
            assertThat(hashMap(),
                       not(equivalentTo(hashMap(tuple("foo", 1), tuple("bar", 2)), sameKeys)));
        }
    }

    public static final class HashingAlgorithms {

        @Test
        public void entries() {
            HashingAlgorithm<HashMap<String, Integer>> entries =
                    Map.HashingAlgorithms.entries(objectHashCode(), objectHashCode());

            assertThat(hashMap(), hashesEquivalentlyTo(hashMap(), entries));
            assertThat(hashMap(tuple("foo", 1)), hashesEquivalentlyTo(hashMap(tuple("foo", 1)), entries));

            assertThat(hashMap(tuple("foo", 1)), not(hashesEquivalentlyTo(hashMap(), entries)));
            assertThat(hashMap(tuple("foo", 1)), not(hashesEquivalentlyTo(hashMap(tuple("bar", 2)), entries)));

            assertThat(hashMap(tuple("foo", 1), tuple("bar", 2)).remove("bar"),
                       hashesEquivalentlyTo(hashMap(tuple("foo", 1)), entries));
        }
    }
}