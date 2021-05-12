package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.testsupport.DefaultMethodsSet;
import com.jnape.palatable.shoki.testsupport.StubbedHashingAlgorithm;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

@RunWith(Enclosed.class)
public class SetTest {

    public static final class DefaultMethods {

        @Test
        public void addAll() {
            DefaultMethodsSet<Natural, Integer> empty = DefaultMethodsSet.delegate(hashSet());
            DefaultMethodsSet<Natural, Integer> _123  = DefaultMethodsSet.delegate(hashSet(1, 2, 3));
            DefaultMethodsSet<Natural, Integer> _234  = DefaultMethodsSet.delegate(hashSet(2, 3, 4));
            DefaultMethodsSet<Natural, Integer> _1234 = DefaultMethodsSet.delegate(hashSet(1, 2, 3, 4));

            assertThat(empty, equivalentTo(empty.addAll(empty), sameElements()));
            assertThat(_123, equivalentTo(empty.addAll(_123), sameElements()));
            assertThat(_123, equivalentTo(_123.addAll(empty), sameElements()));
            assertThat(_123, equivalentTo(_123.addAll(_123), sameElements()));
            assertThat(_1234, equivalentTo(_123.addAll(_234), sameElements()));
            assertThat(_1234, equivalentTo(_234.addAll(_123), sameElements()));
        }

        @Test
        public void intersection() {
            DefaultMethodsSet<Natural, Integer> empty = DefaultMethodsSet.delegate(hashSet());
            DefaultMethodsSet<Natural, Integer> _123  = DefaultMethodsSet.delegate(hashSet(1, 2, 3));
            DefaultMethodsSet<Natural, Integer> _234  = DefaultMethodsSet.delegate(hashSet(2, 3, 4));
            DefaultMethodsSet<Natural, Integer> _23   = DefaultMethodsSet.delegate(hashSet(2, 3));

            assertThat(empty, equivalentTo(empty.intersection(empty), sameElements()));
            assertThat(empty, equivalentTo(empty.intersection(_123), sameElements()));
            assertThat(empty, equivalentTo(_123.intersection(empty), sameElements()));
            assertThat(_123, equivalentTo(_123.intersection(_123), sameElements()));
            assertThat(_23, equivalentTo(_123.intersection(_234), sameElements()));
            assertThat(_23, equivalentTo(_234.intersection(_123), sameElements()));
        }

        @Test
        public void union() {
            DefaultMethodsSet<Natural, Integer> empty = DefaultMethodsSet.delegate(hashSet());
            DefaultMethodsSet<Natural, Integer> _123  = DefaultMethodsSet.delegate(hashSet(1, 2, 3));
            DefaultMethodsSet<Natural, Integer> _234  = DefaultMethodsSet.delegate(hashSet(2, 3, 4));
            DefaultMethodsSet<Natural, Integer> _1234 = DefaultMethodsSet.delegate(hashSet(1, 2, 3, 4));

            assertThat(empty, equivalentTo(empty.union(empty), sameElements()));
            assertThat(_123, equivalentTo(empty.union(_123), sameElements()));
            assertThat(_123, equivalentTo(_123.union(empty), sameElements()));
            assertThat(_123, equivalentTo(_123.union(_123), sameElements()));
            assertThat(_1234, equivalentTo(_123.union(_234), sameElements()));
            assertThat(_1234, equivalentTo(_234.union(_123), sameElements()));
        }

        @Test
        public void difference() {
            DefaultMethodsSet<Natural, Integer> empty = DefaultMethodsSet.delegate(hashSet());
            DefaultMethodsSet<Natural, Integer> _123  = DefaultMethodsSet.delegate(hashSet(1, 2, 3));
            DefaultMethodsSet<Natural, Integer> _234  = DefaultMethodsSet.delegate(hashSet(2, 3, 4));
            DefaultMethodsSet<Natural, Integer> _1    = DefaultMethodsSet.delegate(hashSet(1));
            DefaultMethodsSet<Natural, Integer> _4    = DefaultMethodsSet.delegate(hashSet(4));

            assertThat(empty, equivalentTo(empty.difference(empty), sameElements()));
            assertThat(empty, equivalentTo(empty.difference(_123), sameElements()));
            assertThat(_123, equivalentTo(_123.difference(empty), sameElements()));
            assertThat(empty, equivalentTo(_123.difference(_123), sameElements()));
            assertThat(_1, equivalentTo(_123.difference(_234), sameElements()));
            assertThat(_4, equivalentTo(_234.difference(_123), sameElements()));
        }

        @Test
        public void symmetricDifference() {
            DefaultMethodsSet<Natural, Integer> empty = DefaultMethodsSet.delegate(hashSet());
            DefaultMethodsSet<Natural, Integer> _123  = DefaultMethodsSet.delegate(hashSet(1, 2, 3));
            DefaultMethodsSet<Natural, Integer> _234  = DefaultMethodsSet.delegate(hashSet(2, 3, 4));
            DefaultMethodsSet<Natural, Integer> _14   = DefaultMethodsSet.delegate(hashSet(1, 4));

            assertThat(empty, equivalentTo(empty.symmetricDifference(empty), sameElements()));
            assertThat(_123, equivalentTo(empty.symmetricDifference(_123), sameElements()));
            assertThat(_123, equivalentTo(_123.symmetricDifference(empty), sameElements()));
            assertThat(empty, equivalentTo(_123.symmetricDifference(_123), sameElements()));
            assertThat(_14, equivalentTo(_123.symmetricDifference(_234), sameElements()));
            assertThat(_14, equivalentTo(_234.symmetricDifference(_123), sameElements()));
        }
    }

    public static final class EquivalenceRelations {

        @Test
        public void sameElements() {
            EquivalenceRelation<Set<?, Integer>> sameElements = Set.EquivalenceRelations.sameElements();

            assertThat(hashSet(), equivalentTo(hashSet(), sameElements));
            assertThat(hashSet(1, 2), equivalentTo(hashSet(1, 2), sameElements));
            assertThat(hashSet(1, 2), equivalentTo(hashSet(2, 1), sameElements));
            assertThat(hashSet(1, 2), not(equivalentTo(hashSet(), sameElements)));
            assertThat(hashSet(), not(equivalentTo(hashSet(1, 2), sameElements)));

            assertThat(hashSet(1, 2, 3),
                       equivalentTo(hashSet(objectEquals(),
                                            StubbedHashingAlgorithm.<Integer>stubbedHashingAlgorithm()
                                                    .stub(1, 3)
                                                    .stub(2, 2)
                                                    .stub(3, 1),
                                            1, 2, 3),
                                    sameElements));
        }
    }
}