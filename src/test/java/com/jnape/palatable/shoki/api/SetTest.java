package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsSet;
import com.jnape.palatable.shoki.testsupport.StubbedHashingAlgorithm;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

@RunWith(Enclosed.class)
public class SetTest {

    public static final class DefaultMethods {

        @Test
        public void symmetricDifference() {
            DefaultMethodsSet<Natural, Integer> empty = DefaultMethodsSet.delegate(HashSet.empty());
            DefaultMethodsSet<Natural, Integer> _123  = DefaultMethodsSet.delegate(HashSet.of(1, 2, 3));
            DefaultMethodsSet<Natural, Integer> _234  = DefaultMethodsSet.delegate(HashSet.of(2, 3, 4));
            DefaultMethodsSet<Natural, Integer> _14   = DefaultMethodsSet.delegate(HashSet.of(1, 4));

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

            assertThat(HashSet.empty(), equivalentTo(HashSet.empty(), sameElements));
            assertThat(HashSet.of(1, 2), equivalentTo(HashSet.of(1, 2), sameElements));
            assertThat(HashSet.of(1, 2), equivalentTo(HashSet.of(2, 1), sameElements));
            assertThat(HashSet.of(1, 2), not(equivalentTo(HashSet.empty(), sameElements)));
            assertThat(HashSet.empty(), not(equivalentTo(HashSet.of(1, 2), sameElements)));

            assertThat(HashSet.of(1, 2, 3),
                       equivalentTo(HashSet.of(objectEquals(),
                                               StubbedHashingAlgorithm.<Integer>stubbedHashingAlgorithm()
                                                       .stub(1, 3)
                                                       .stub(2, 2)
                                                       .stub(3, 1),
                                               1, 2, 3),
                                    sameElements));
        }
    }
}