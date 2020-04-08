package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsMap;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.Map.EquivalenceRelations.sameEntries;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.impl.HashMap.empty;
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
            DefaultMethodsMap<Natural, String, Integer> empty = DefaultMethodsMap.delegate(empty());

            assertThat(empty.merge(empty, sum),
                       equivalentTo(empty, sameEntries));
            assertThat(empty.merge(HashMap.of(tuple("foo", 1)), sum),
                       equivalentTo(HashMap.of(tuple("foo", 1)), sameEntries));
            assertThat(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1))).merge(empty, sum),
                       equivalentTo(HashMap.of(tuple("foo", 1)), sameEntries));
            assertThat(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1), tuple("bar", 2)))
                               .merge(HashMap.of(tuple("foo", 1), tuple("baz", 3)), sum),
                       equivalentTo(HashMap.of(tuple("foo", 2), tuple("bar", 2), tuple("baz", 3)), sameEntries));
        }


        @Test
        public void removeAll() {
            EquivalenceRelation<Map<Natural, String, Integer>> sameEntries = sameEntries(objectEquals());
            DefaultMethodsMap<Natural, String, Integer>        empty       = DefaultMethodsMap.delegate(empty());

            assertThat(empty.removeAll(HashSet.empty()),
                       equivalentTo(empty, sameEntries));
            assertThat(empty.removeAll(HashSet.of("foo", "bar")),
                       equivalentTo(empty, sameEntries));
            assertThat(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1))).removeAll(HashSet.empty()),
                       equivalentTo(HashMap.of(tuple("foo", 1)), sameEntries));
            assertThat(DefaultMethodsMap.delegate(HashMap.of(tuple("foo", 1), tuple("bar", 2)))
                               .removeAll(HashSet.of("foo", "baz")),
                       equivalentTo(HashMap.of(tuple("bar", 2)), sameEntries));
        }
    }

    public static final class EquivalenceRelations {

        @Test
        public void sameEntries() {
            EquivalenceRelation<HashMap<String, Integer>> sameEntries =
                    Map.EquivalenceRelations.sameEntries(objectEquals());

            assertThat(HashMap.empty(), equivalentTo(HashMap.empty(), sameEntries));
            assertThat(HashMap.of(tuple("foo", 1), tuple("bar", 2)),
                       equivalentTo(HashMap.of(tuple("foo", 1), tuple("bar", 2)), sameEntries));
            assertThat(HashMap.empty(),
                       not(equivalentTo(HashMap.of(tuple("foo", 1), tuple("bar", 2)), sameEntries)));
        }

        @Test
        public void sameKeys() {
            EquivalenceRelation<HashMap<String, Integer>> sameKeys =
                    Map.EquivalenceRelations.sameKeys(sameElements());

            assertThat(HashMap.empty(), equivalentTo(HashMap.empty(), sameKeys));
            assertThat(HashMap.of(tuple("foo", 1), tuple("bar", 2)),
                       equivalentTo(HashMap.of(tuple("foo", 2), tuple("bar", 1)), sameKeys));
            assertThat(HashMap.empty(),
                       not(equivalentTo(HashMap.of(tuple("foo", 1), tuple("bar", 2)), sameKeys)));
        }
    }
}