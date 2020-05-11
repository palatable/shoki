package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsMultiSet;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.lambda.functions.Fn1.fn1;
import static com.jnape.palatable.lambda.functions.builtin.fn3.CmpEqBy.cmpEqBy;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.HashMultiSet.hashMultiSet;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

@RunWith(Enclosed.class)
public class MultiSetTest {
    public static final class DefaultMethods {
        @Test
        public void removeAll() {
            assertTrue(DefaultMethodsMultiSet.<String>delegate(hashMultiSet()).remove("foo").isEmpty());
            assertEquals(zero(),
                         DefaultMethodsMultiSet.delegate(hashMultiSet("foo", "bar", "foo")).remove("foo")
                                 .get("foo"));
            assertEquals(known(one()),
                         DefaultMethodsMultiSet.delegate(hashMultiSet("foo", "bar", "foo")).remove("foo")
                                 .sizeInfo());
        }

        @Test
        public void removeOne() {
            assertTrue(DefaultMethodsMultiSet.<String>delegate(hashMultiSet()).dec("foo").isEmpty());
            assertEquals(one(),
                         DefaultMethodsMultiSet.delegate(hashMultiSet("foo", "foo")).dec("foo").get("foo"));
            assertEquals(known(abs(2)),
                         DefaultMethodsMultiSet.delegate(hashMultiSet("foo", "bar", "foo")).dec("foo")
                                 .sizeInfo());
        }

        @Test
        public void addOne() {
            assertEquals(one(), DefaultMethodsMultiSet.delegate(HashMultiSet.<String>hashMultiSet()).inc("foo").get("foo"));
        }

        @Test
        public void addAll() {
            assertTrue(DefaultMethodsMultiSet.<String>delegate(hashMultiSet())
                               .sum(hashMultiSet()).isEmpty());

            MultiSet<String> addAll = DefaultMethodsMultiSet.<String>delegate(hashMultiSet())
                    .sum(hashMultiSet("foo", "foo", "bar"));
            assertEquals(abs(2), addAll.get("foo"));
            assertEquals(abs(1), addAll.get("bar"));
            assertEquals(known(abs(3)), addAll.sizeInfo());
        }

        @Test
        public void merge() {
            MultiSet<String> merge = DefaultMethodsMultiSet.delegate(hashMultiSet("a", "b"))
                    .merge(DefaultMethodsMultiSet.delegate(hashMultiSet("b", "c")), Natural::plus);
            assertEquals(one(), merge.get("a"));
            assertEquals(abs(2), merge.get("b"));
            assertEquals(one(), merge.get("c"));
            assertEquals(known(abs(4)), merge.sizeInfo());
        }

        @Test
        public void union() {
            DefaultMethodsMultiSet<String> first  = DefaultMethodsMultiSet.delegate(hashMultiSet("a", "a", "b"));
            DefaultMethodsMultiSet<String> second = DefaultMethodsMultiSet.delegate(hashMultiSet("b", "c"));
            MultiSet<String> union = first
                    .union(second);

            assertEquals(abs(2), union.get("a"));
            assertEquals(one(), union.get("b"));
            assertEquals(one(), union.get("c"));
            assertEquals(known(abs(4)), union.sizeInfo());

            assertThat(first.union(first),
                       equivalentTo(first, MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(first.union(hashMultiSet()),
                       equivalentTo(first, MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(second.union(second),
                       equivalentTo(second, MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(second.union(hashMultiSet()),
                       equivalentTo(second, MultiSet.EquivalenceRelations.elementMultiplicity()));
        }

        @Test
        public void intersection() {
            DefaultMethodsMultiSet<String> first = DefaultMethodsMultiSet.delegate(hashMultiSet("a", "b", "b", "c"));
            DefaultMethodsMultiSet<String> second = DefaultMethodsMultiSet
                    .delegate(hashMultiSet("b", "b", "c", "c"));
            MultiSet<String> union = first
                    .intersection(second);

            assertEquals(zero(), union.get("a"));
            assertEquals(abs(2), union.get("b"));
            assertEquals(one(), union.get("c"));
            assertEquals(known(abs(3)), union.sizeInfo());

            assertThat(first.intersection(first),
                       equivalentTo(first, MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(first.intersection(hashMultiSet()),
                       equivalentTo(hashMultiSet(), MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(second.intersection(second),
                       equivalentTo(second, MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(second.intersection(hashMultiSet()),
                       equivalentTo(hashMultiSet(), MultiSet.EquivalenceRelations.elementMultiplicity()));
        }

        @Test
        public void inclusion() {
            assertTrue(DefaultMethodsMultiSet.delegate(hashMultiSet()).inclusion(hashMultiSet()));
            assertTrue(DefaultMethodsMultiSet.delegate(hashMultiSet("a")).inclusion(hashMultiSet()));
            assertTrue(DefaultMethodsMultiSet.delegate(hashMultiSet("a")).inclusion(hashMultiSet("a")));
            assertTrue(DefaultMethodsMultiSet.delegate(hashMultiSet("a", "a")).inclusion(hashMultiSet("a", "a")));
            assertTrue(DefaultMethodsMultiSet.delegate(hashMultiSet("a", "b")).inclusion(hashMultiSet("a")));
            assertTrue(DefaultMethodsMultiSet.delegate(hashMultiSet("a", "b")).inclusion(hashMultiSet("a", "b")));

            assertFalse(DefaultMethodsMultiSet.delegate(hashMultiSet("a")).inclusion(hashMultiSet("a", "a")));
            assertFalse(DefaultMethodsMultiSet.delegate(hashMultiSet("a")).inclusion(hashMultiSet("b")));
        }

        @Test
        public void contains() {
            assertTrue(DefaultMethodsMultiSet.delegate(hashMultiSet("a")).contains("a"));
            assertFalse(DefaultMethodsMultiSet.delegate(hashMultiSet()).contains("a"));
        }

        @Test
        public void symmetricDifference() {
            assertThat(DefaultMethodsMultiSet.delegate(hashMultiSet("a", "b", "c"))
                               .symmetricDifference(hashMultiSet("b")),
                       equivalentTo(hashMultiSet("a", "c"),
                                    MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(DefaultMethodsMultiSet.delegate(hashMultiSet("a", "a", "a"))
                               .symmetricDifference(hashMultiSet("a")),
                       equivalentTo(hashMultiSet("a", "a"),
                                    MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(DefaultMethodsMultiSet.delegate(hashMultiSet("a"))
                               .symmetricDifference(hashMultiSet("a")),
                       equivalentTo(hashMultiSet(),
                                    MultiSet.EquivalenceRelations.elementMultiplicity()));
        }
    }

    public static final class EquivalenceRelations {

        @Test
        public void sameElements() {
            assertThat(DefaultMethodsMultiSet.delegate(hashMultiSet()),
                       equivalentTo(hashMultiSet(),
                                    MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(DefaultMethodsMultiSet.delegate(hashMultiSet("a")),
                       equivalentTo(hashMultiSet("a"),
                                    MultiSet.EquivalenceRelations.elementMultiplicity()));
            assertThat(DefaultMethodsMultiSet.delegate(hashMultiSet("a", "a")),
                       not(equivalentTo(hashMultiSet("a"),
                                        MultiSet.EquivalenceRelations.elementMultiplicity())));
            assertThat(DefaultMethodsMultiSet.delegate(hashMultiSet(cmpEqBy(String::length)::apply,
                                                                    fn1(String::length)
                                                                            .fmap(integer -> Integer
                                                                                    .hashCode(
                                                                                            integer))::apply,
                                                                    "a", "a")),
                       equivalentTo(hashMultiSet("b", "b"),
                                    MultiSet.EquivalenceRelations.elementMultiplicity()));
        }
    }

}