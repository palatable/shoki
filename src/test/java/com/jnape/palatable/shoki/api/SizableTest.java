package com.jnape.palatable.shoki.api;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.shoki.api.SizeInfo.finite;
import static com.jnape.palatable.shoki.api.SizeInfo.infinite;
import static com.jnape.palatable.shoki.api.SizeInfo.unsized;
import static com.jnape.palatable.shoki.api.Value.known;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

@RunWith(Enclosed.class)
public class SizableTest {

    public static final class EquivalenceRelations {

        @Test
        public void sameSizes() {
            EquivalenceRelation<Sizable> sameSizes = Sizable.EquivalenceRelations.sizeInfos();

            Sizable finite   = () -> known(finite(0));
            Sizable infinite = () -> known(infinite());
            Sizable unsized  = () -> known(unsized());

            assertThat(finite, equivalentTo(finite, sameSizes));
            assertThat(infinite, equivalentTo(infinite, sameSizes));
            assertThat(unsized, equivalentTo(unsized, sameSizes));
            assertThat(finite, not(equivalentTo(unsized, sameSizes)));
            assertThat(finite, not(equivalentTo(infinite, sameSizes)));
        }
    }
}