package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.shoki.api.SizeInfo.Sized;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.shoki.api.SizeInfo.finite;
import static com.jnape.palatable.shoki.api.SizeInfo.infinite;
import static com.jnape.palatable.shoki.api.SizeInfo.unsized;
import static com.jnape.palatable.shoki.api.Value.computedEveryTime;
import static com.jnape.palatable.shoki.api.Value.computedOnce;
import static com.jnape.palatable.shoki.api.Value.known;
import static com.jnape.palatable.shoki.testsupport.Atom.atom;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

@RunWith(Enclosed.class)
public class SizeInfoTest {

    @RunWith(Enclosed.class)
    public static final class SizedTest {

        public static final class FiniteTest {

            @Test
            public void coproduct() {
                Sized.Finite<Integer> finite = finite(known(1));
                assertEquals(just(finite),
                             finite.projectA().fmap(Sized::cardinality).flatMap(CoProduct2::projectA));
            }

            @Test
            public void string() {
                Value.Known<Integer> knownValue = known(1);
                assertEquals("SizeInfo.Sized.Finite[" + knownValue + "]", finite(knownValue).toString());

                Value.Computed.Once<Integer> computedOnce = computedOnce(atom(), () -> 1);
                assertEquals("SizeInfo.Sized.Finite[" + computedOnce + "]", finite(computedOnce).toString());

                Value.Computed.EveryTime<Integer> computedEveryTime = computedEveryTime(() -> 1);
                assertEquals("SizeInfo.Sized.Finite[" + computedEveryTime + "]", finite(computedEveryTime).toString());
            }
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