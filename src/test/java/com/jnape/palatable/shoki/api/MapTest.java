package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsMap;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.Map.EquivalenceRelations.sameEntries;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.impl.HashMap.hashMap;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

@RunWith(Enclosed.class)
public class MapTest {

    public static final class DefaultMethods {

        @Test
        public void putAllWithSemigroup() {
            EquivalenceRelation<Map<Natural, String, Integer>> sameEntries = sameEntries(objectEquals());

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
            EquivalenceRelation<Map<Natural, String, Integer>> sameEntries = sameEntries(objectEquals());
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
                    Map.EquivalenceRelations.sameEntries(objectEquals());

            assertThat(hashMap(), equivalentTo(hashMap(), sameEntries));
            assertThat(hashMap(tuple("foo", 1), tuple("bar", 2)),
                       equivalentTo(hashMap(tuple("foo", 1), tuple("bar", 2)), sameEntries));
            assertThat(hashMap(),
                       not(equivalentTo(hashMap(tuple("foo", 1), tuple("bar", 2)), sameEntries)));
        }

        @Test
        public void sameKeys() {
            EquivalenceRelation<HashMap<String, Integer>> sameKeys =
                    Map.EquivalenceRelations.sameKeys(sameElements());

            assertThat(hashMap(), equivalentTo(hashMap(), sameKeys));
            assertThat(hashMap(tuple("foo", 1), tuple("bar", 2)),
                       equivalentTo(hashMap(tuple("foo", 2), tuple("bar", 1)), sameKeys));
            assertThat(hashMap(),
                       not(equivalentTo(hashMap(tuple("foo", 1), tuple("bar", 2)), sameKeys)));
        }
    }
}