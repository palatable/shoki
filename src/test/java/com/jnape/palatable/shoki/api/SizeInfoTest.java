package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.shoki.api.SizeInfo.Sized;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.shoki.api.SizeInfo.infinite;
import static com.jnape.palatable.shoki.api.SizeInfo.unsized;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

@RunWith(Enclosed.class)
public class SizeInfoTest {

    @RunWith(Enclosed.class)
    public static final class SizedTest {

        @RunWith(Enclosed.class)
        public static final class FiniteTest {

        }

        public static final class InfiniteTest {

            @Test
            public void singleton() {
                assertSame(infinite(), infinite());
            }

            @Test
            public void coproduct() {
                assertEquals(just(infinite()),
                             infinite().projectA().fmap(Sized::cardinality).flatMap(CoProduct2::projectB));
            }

            @Test
            public void string() {
                assertEquals("SizeInfo.Sized.Infinite", infinite().toString());
            }
        }
    }

    public static final class UnsizedTest {
        @Test
        public void singleton() {
            assertSame(unsized(), unsized());
        }

        @Test
        public void coproduct() {
            assertEquals(just(unsized()), unsized().projectB());
        }

        @Test
        public void string() {
            assertEquals("SizeInfo.Unsized", unsized().toString());
        }
    }
}