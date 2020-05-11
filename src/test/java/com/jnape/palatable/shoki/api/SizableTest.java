package com.jnape.palatable.shoki.api;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

@RunWith(Enclosed.class)
public class SizableTest {

    public static final class EquivalenceRelations {

        @Test
        public void sameSizes() {
            EquivalenceRelation<Sizable> sameSizes = Sizable.EquivalenceRelations.sizeInfos();

            Sizable knownEmpty    = () -> known(0);
            Sizable knownNonEmpty = () -> known(1);
            Sizable unknown       = () -> known(1);

            assertThat(knownEmpty, equivalentTo(knownEmpty, sameSizes));
            assertThat(knownNonEmpty, equivalentTo(knownNonEmpty, sameSizes));
            assertThat(unknown, equivalentTo(unknown, sameSizes));
            assertThat(knownEmpty, not(equivalentTo(unknown, sameSizes)));
            assertThat(knownEmpty, not(equivalentTo(knownNonEmpty, sameSizes)));
        }
    }
}