package com.jnape.palatable.shoki;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.atomic.AtomicInteger;

import static com.jnape.palatable.shoki.$.$;
import static org.junit.Assert.assertEquals;

public class $Test {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void memoizationPreventsRecomputation() {
        AtomicInteger counter = new AtomicInteger(0);
        $<Integer> memoized = $(counter::incrementAndGet).memoize();

        assertEquals((Integer) 1, memoized.force());
        assertEquals((Integer) 1, memoized.force());
    }

    @Test
    public void onlyOneThreadAllowedToComputeResultToMemoize() throws Exception {
        AtomicInteger counter = new AtomicInteger(0);
        $<Integer> memoized = $(counter::incrementAndGet).memoize();

        int threadCount = 100;
        CyclicBarrier barrier = new CyclicBarrier(threadCount);
        CountDownLatch latch = new CountDownLatch(threadCount);
        while (threadCount-- > 0)
            new Thread(() -> {
                try {
                    barrier.await();
                    memoized.force();
                    latch.countDown();
                } catch (Exception e) {
                    e.printStackTrace();
                    throw new AssertionError(e.getMessage());
                }
            }) {{
                start();
            }};

        latch.await();

        assertEquals((Integer) 1, memoized.force());
        assertEquals(1, counter.get());
    }
}