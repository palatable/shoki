package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.shoki.api.Value.ComputedEveryTime;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import java.util.concurrent.atomic.AtomicInteger;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.Value.computedEveryTime;
import static com.jnape.palatable.shoki.api.Value.known;
import static com.jnape.palatable.shoki.api.Value.memoized;
import static com.jnape.palatable.shoki.testsupport.Atom.atom;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

@RunWith(Enclosed.class)
public class ValueTest {

    @RunWith(Enclosed.class)
    public static final class ComputedAtMostOnceTest {

        public static final class KnownTest {

            @Test
            public void getSize() {
                assertEquals((Integer) 1, known(1).get());
                assertEquals(zero(), known(zero()).get());
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
                Value.ComputedAtMostOnce.Known<Integer> known = known(1);
                assertEquals(just(known),
                             known.projectA()
                                     .fmap(Value.ComputedAtMostOnce::evaluation)
                                     .flatMap(CoProduct2::projectA));
            }

            @Test
            public void string() {
                assertEquals("Value.ComputedAtMostOnce.Known[1]", known(1).toString());
            }
        }

        public static final class MemoizedTest {

            @Test
            public void computesOnceAndMemoizesResult() {
                Value.ComputedAtMostOnce.Memoized<Integer> memoized =
                        memoized(atom(), new AtomicInteger(0)::incrementAndGet);
                assertEquals((Integer) 1, memoized.getOrCompute());
                assertEquals((Integer) 1, memoized.getOrCompute());
            }

            @Test
            public void coproduct() {
                Value.ComputedAtMostOnce.Memoized<Integer> memoized = memoized(atom(), () -> 1);
                assertEquals(just(memoized),
                             memoized.projectA()
                                     .fmap(Value.ComputedAtMostOnce::evaluation)
                                     .flatMap(CoProduct2::projectB));
            }

            @Test
            public void string() {
                Value.ComputedAtMostOnce.Memoized<Integer> memoized = memoized(atom(), () -> 1);
                assertEquals("Value.ComputedAtMostOnce.Memoized[…]", memoized.toString());
                memoized.getOrCompute();
                assertEquals("Value.ComputedAtMostOnce.Memoized[1]", memoized.toString());
            }
        }
    }

    public static final class ComputedEveryTimeTest {

        @Test
        public void computesAlways() {
            ComputedEveryTime<Integer> computedEveryTime =
                    computedEveryTime(new AtomicInteger(0)::incrementAndGet);
            assertEquals((Integer) 1, computedEveryTime.getOrCompute());
            assertEquals((Integer) 2, computedEveryTime.getOrCompute());
        }

        @Test
        public void coproduct() {
            ComputedEveryTime.ComputedEveryTime<Integer> computedEveryTime = computedEveryTime(() -> 1);
            assertEquals(just(computedEveryTime), computedEveryTime.projectB());
        }

        @Test
        public void string() {
            assertEquals("Value.ComputedEveryTime[…]", computedEveryTime(() -> 1).toString());
        }
    }
}