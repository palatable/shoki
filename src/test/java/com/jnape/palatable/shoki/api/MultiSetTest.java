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
import static com.jnape.palatable.shoki.impl.HashMultiSet.empty;
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
            assertTrue(DefaultMethodsMultiSet.<String>delegate(empty()).removeAll("foo").isEmpty());
            assertEquals(zero(),
                         DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).removeAll("foo")
                                 .get("foo"));
            assertEquals(known(one()),
                         DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).removeAll("foo")
                                 .sizeInfo());
        }

        @Test
        public void removeOne() {
            assertTrue(DefaultMethodsMultiSet.<String>delegate(empty()).remove("foo").isEmpty());
            assertEquals(one(),
                         DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "foo")).remove("foo").get("foo"));
            assertEquals(known(abs(2)),
                         DefaultMethodsMultiSet.delegate(HashMultiSet.of("foo", "bar", "foo")).remove("foo")
                                 .sizeInfo());
        }

        @Test
        public void addOne() {
            assertEquals(one(), DefaultMethodsMultiSet.delegate(HashMultiSet.<String>empty()).add("foo").get("foo"));
        }

        @Test
        public void addAll() {
            assertTrue(DefaultMethodsMultiSet.<String>delegate(empty())
                               .addAll(empty()).isEmpty());

            MultiSet<String> addAll = DefaultMethodsMultiSet.<String>delegate(empty())
                    .addAll(HashMultiSet.of("foo", "foo", "bar"));
            assertEquals(abs(2), addAll.get("foo"));
            assertEquals(abs(1), addAll.get("bar"));
            assertEquals(known(abs(3)), addAll.sizeInfo());
        }

        @Test
        public void merge() {
            MultiSet<String> merge = DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "b"))
                    .merge(Natural::plus, DefaultMethodsMultiSet.delegate(HashMultiSet.of("b", "c")));
            assertEquals(one(), merge.get("a"));
            assertEquals(abs(2), merge.get("b"));
            assertEquals(one(), merge.get("c"));
            assertEquals(known(abs(4)), merge.sizeInfo());
        }

        @Test
        public void union() {
            DefaultMethodsMultiSet<String> first  = DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "a", "b"));
            DefaultMethodsMultiSet<String> second = DefaultMethodsMultiSet.delegate(HashMultiSet.of("b", "c"));
            MultiSet<String> union = first
                    .union(second);

            assertEquals(abs(2), union.get("a"));
            assertEquals(one(), union.get("b"));
            assertEquals(one(), union.get("c"));
            assertEquals(known(abs(4)), union.sizeInfo());

            assertThat(first.union(first), equivalentTo(first, MultiSet.EquivalenceRelations.sameElements()));
            assertThat(first.union(empty()), equivalentTo(first, MultiSet.EquivalenceRelations.sameElements()));
            assertThat(second.union(second), equivalentTo(second, MultiSet.EquivalenceRelations.sameElements()));
            assertThat(second.union(empty()), equivalentTo(second, MultiSet.EquivalenceRelations.sameElements()));
        }

        @Test
        public void intersection() {
            DefaultMethodsMultiSet<String> first = DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "b", "b", "c"));
            DefaultMethodsMultiSet<String> second = DefaultMethodsMultiSet
                    .delegate(HashMultiSet.of("b", "b", "c", "c"));
            MultiSet<String> union = first
                    .intersection(second);

            assertEquals(zero(), union.get("a"));
            assertEquals(abs(2), union.get("b"));
            assertEquals(one(), union.get("c"));
            assertEquals(known(abs(3)), union.sizeInfo());

            assertThat(first.intersection(first), equivalentTo(first, MultiSet.EquivalenceRelations.sameElements()));
            assertThat(first.intersection(empty()),
                       equivalentTo(empty(), MultiSet.EquivalenceRelations.sameElements()));
            assertThat(second.intersection(second), equivalentTo(second, MultiSet.EquivalenceRelations.sameElements()));
            assertThat(second.intersection(empty()),
                       equivalentTo(empty(), MultiSet.EquivalenceRelations.sameElements()));
        }

        @Test
        public void inclusion() {
            assertTrue(DefaultMethodsMultiSet.delegate(HashMultiSet.empty()).inclusion(HashMultiSet.empty()));
            assertTrue(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a")).inclusion(HashMultiSet.empty()));
            assertTrue(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a")).inclusion(HashMultiSet.of("a")));
            assertTrue(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "a")).inclusion(HashMultiSet.of("a", "a")));
            assertTrue(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "b")).inclusion(HashMultiSet.of("a")));
            assertTrue(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "b")).inclusion(HashMultiSet.of("a", "b")));

            assertFalse(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a")).inclusion(HashMultiSet.of("a", "a")));
            assertFalse(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a")).inclusion(HashMultiSet.of("b")));
        }

        @Test
        public void contains() {
            assertTrue(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a")).contains("a"));
            assertFalse(DefaultMethodsMultiSet.delegate(HashMultiSet.empty()).contains("a"));
        }

        @Test
        public void symmetricDifference() {
            assertThat(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "b", "c"))
                               .symmetricDifference(HashMultiSet.of("b")),
                       equivalentTo(HashMultiSet.of("a", "c"), MultiSet.EquivalenceRelations.sameElements()));
            assertThat(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "a", "a"))
                               .symmetricDifference(HashMultiSet.of("a")),
                       equivalentTo(HashMultiSet.of("a", "a"), MultiSet.EquivalenceRelations.sameElements()));
            assertThat(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a"))
                               .symmetricDifference(HashMultiSet.of("a")),
                       equivalentTo(HashMultiSet.empty(), MultiSet.EquivalenceRelations.sameElements()));
        }
    }

    public static final class EquivalenceRelations {

        @Test
        public void sameElements() {
            assertThat(DefaultMethodsMultiSet.delegate(HashMultiSet.empty()),
                       equivalentTo(HashMultiSet.empty(), MultiSet.EquivalenceRelations.sameElements()));
            assertThat(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a")),
                       equivalentTo(HashMultiSet.of("a"), MultiSet.EquivalenceRelations.sameElements()));
            assertThat(DefaultMethodsMultiSet.delegate(HashMultiSet.of("a", "a")),
                       not(equivalentTo(HashMultiSet.of("a"), MultiSet.EquivalenceRelations.sameElements())));
            assertThat(DefaultMethodsMultiSet.delegate(HashMultiSet.of(cmpEqBy(String::length)::apply,
                                                                       fn1(String::length)
                                                                               .fmap(integer -> Integer
                                                                                       .hashCode(
                                                                                               integer))::apply,
                                                                       "a", "a")),
                       equivalentTo(HashMultiSet.of("b", "b"),
                                    MultiSet.EquivalenceRelations.sameElements()));
        }
    }

}