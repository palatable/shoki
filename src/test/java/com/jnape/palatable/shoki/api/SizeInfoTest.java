package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.shoki.api.SizeInfo.Sized;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import java.util.concurrent.atomic.AtomicInteger;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.computedEveryTime;
import static com.jnape.palatable.shoki.api.SizeInfo.computedOnce;
import static com.jnape.palatable.shoki.api.SizeInfo.infinite;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.api.SizeInfo.unsized;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;

@RunWith(Enclosed.class)
public class SizeInfoTest {

    @RunWith(Enclosed.class)
    public static final class SizedTest {

        @RunWith(Enclosed.class)
        public static final class FiniteTest {

            public static final class KnownTest {

                @Test
                public void getSize() {
                    assertEquals((Integer) 1, known(1).getSize());
                    assertEquals(zero(), known(zero()).getSize());
                }

                @Test
                public void equalityAndHashing() {
                    assertEquals(known(1), known(1));
                    assertNotEquals(known(1), known(2));
                    assertNotEquals(known(1), new Object());

                    assertEquals(known(1).hashCode(), known(1).hashCode());
                    assertNotEquals(known(1).hashCode(), known(2).hashCode());
                }

                @Test
                public void coproduct() {
                    assertEquals(just(known(1)),
                                 known(1).projectA()
                                         .fmap(Sized::cardinality).flatMap(CoProduct2::projectA)
                                         .fmap(Finite::availability).flatMap(CoProduct2::projectA));
                }

                @Test
                public void string() {
                    assertEquals("SizeInfo.Finite.Known[1]", known(1).toString());
                }
            }

            @RunWith(Enclosed.class)
            public static final class ComputedTest {

                public static final class OnceTest {

                    @Test
                    public void computesOnceAndMemoizesResult() {
                        Finite.Computed.Once<Integer> computedOnce =
                                computedOnce(new AtomicInteger(0)::incrementAndGet);
                        assertEquals((Integer) 1, computedOnce.getOrCompute());
                        assertEquals((Integer) 1, computedOnce.getOrCompute());
                    }

                    @Test
                    public void coproduct() {
                        Finite.Computed.Once<Integer> computedOnce = computedOnce(() -> 1);
                        assertEquals(just(computedOnce),
                                     computedOnce.projectA()
                                             .fmap(Sized::cardinality).flatMap(CoProduct2::projectA)
                                             .fmap(Finite::availability).flatMap(CoProduct2::projectB)
                                             .fmap(Finite.Computed::evaluation).flatMap(CoProduct2::projectA));
                    }

                    @Test
                    public void string() {
                        Finite.Computed.Once<Integer> computedOnce = computedOnce(() -> 1);
                        assertEquals("SizeInfo.Finite.Computed.Once[…]", computedOnce.toString());
                        computedOnce.getOrCompute();
                        assertEquals("SizeInfo.Finite.Computed.Once[1]", computedOnce.toString());
                    }
                }

                public static final class EveryTimeTest {

                    @Test
                    public void computesAlways() {
                        Finite.Computed.EveryTime<Integer> computedEveryTime =
                                computedEveryTime(new AtomicInteger(0)::incrementAndGet);
                        assertEquals((Integer) 1, computedEveryTime.getOrCompute());
                        assertEquals((Integer) 2, computedEveryTime.getOrCompute());
                    }

                    @Test
                    public void coproduct() {
                        Finite.Computed.EveryTime<Integer> computedEveryTime = computedEveryTime(() -> 1);
                        assertEquals(just(computedEveryTime),
                                     computedEveryTime.projectA()
                                             .fmap(Sized::cardinality).flatMap(CoProduct2::projectA)
                                             .fmap(Finite::availability).flatMap(CoProduct2::projectB)
                                             .fmap(Finite.Computed::evaluation).flatMap(CoProduct2::projectB));
                    }

                    @Test
                    public void string() {
                        assertEquals("SizeInfo.Finite.Computed.Always[…]", computedEveryTime(() -> 1).toString());
                    }
                }
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