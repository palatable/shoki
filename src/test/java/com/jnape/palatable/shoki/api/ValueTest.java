package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.shoki.api.Value.Computed;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import java.util.concurrent.atomic.AtomicInteger;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.Value.computedEveryTime;
import static com.jnape.palatable.shoki.api.Value.computedOnce;
import static com.jnape.palatable.shoki.api.Value.known;
import static com.jnape.palatable.shoki.testsupport.Atom.atom;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

@RunWith(Enclosed.class)
public class ValueTest {

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
            assertEquals(just(known(1)), known(1).projectA());
        }

        @Test
        public void string() {
            assertEquals("Value.Known[1]", known(1).toString());
        }
    }

    @RunWith(Enclosed.class)
    public static final class ComputedTest {

        public static final class OnceTest {

            @Test
            public void computesOnceAndMemoizesResult() {
                Computed.Once<Integer> computedOnce =
                        computedOnce(atom(), new AtomicInteger(0)::incrementAndGet);
                assertEquals((Integer) 1, computedOnce.getOrCompute());
                assertEquals((Integer) 1, computedOnce.getOrCompute());
            }

            @Test
            public void coproduct() {
                Computed.Once<Integer> computedOnce = computedOnce(atom(), () -> 1);
                assertEquals(just(computedOnce),
                             computedOnce.projectB()
                                     .fmap(Computed::evaluation).flatMap(CoProduct2::projectA));
            }

            @Test
            public void string() {
                Computed.Once<Integer> computedOnce = computedOnce(atom(), () -> 1);
                assertEquals("Value.Computed.Once[…]", computedOnce.toString());
                computedOnce.getOrCompute();
                assertEquals("Value.Computed.Once[1]", computedOnce.toString());
            }
        }

        public static final class EveryTimeTest {

            @Test
            public void computesAlways() {
                Computed.EveryTime<Integer> computedEveryTime =
                        computedEveryTime(new AtomicInteger(0)::incrementAndGet);
                assertEquals((Integer) 1, computedEveryTime.getOrCompute());
                assertEquals((Integer) 2, computedEveryTime.getOrCompute());
            }

            @Test
            public void coproduct() {
                Computed.EveryTime<Integer> computedEveryTime = computedEveryTime(() -> 1);
                assertEquals(just(computedEveryTime),
                             computedEveryTime.projectB()
                                     .fmap(Computed::evaluation).flatMap(CoProduct2::projectB));
            }

            @Test
            public void string() {
                assertEquals("Value.Computed.EveryTime[…]", computedEveryTime(() -> 1).toString());
            }
        }
    }

}