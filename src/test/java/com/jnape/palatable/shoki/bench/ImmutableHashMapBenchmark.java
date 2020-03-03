package com.jnape.palatable.shoki.bench;

import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.shoki.ImmutableHashMap;

import java.util.HashMap;

import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static java.lang.String.join;
import static java.util.Arrays.asList;

public class ImmutableHashMapBenchmark {

    private static final int ITERATIONS = 10;

    private interface BlackHole {
        void accept(Object o);

        BlackHole BLACK_HOLE = o -> {};
    }

    private static synchronized double benchmark_j_u_HashMap() {
        HashMap<Integer, Unit> m;
        long                   start = System.nanoTime();
        for (int i = 0; i < ITERATIONS; i++) {
            m = new HashMap<>();
            for (int j = 0; j < 1_000_000; j++) {
                m.put(j, UNIT);
            }
            BlackHole.BLACK_HOLE.accept(m);
        }
        long end = System.nanoTime();
        return (end - start) / (double) ITERATIONS;
    }

    private static synchronized double benchmark_ImmutableHashMap() {
        long start = System.nanoTime();
        for (int i = 0; i < ITERATIONS; i++) {
            ImmutableHashMap<Integer, Unit> m = ImmutableHashMap.empty();
            for (int j = 0; j < 1_000_000; j++) {
                m = m.put(j, UNIT);
            }
            BlackHole.BLACK_HOLE.accept(m);
        }
        long end = System.nanoTime();
        return (end - start) / (double) ITERATIONS;
    }

    public static void main(String[] args) {
        System.out.println("Warming up...");
        benchmark_j_u_HashMap();
        benchmark_ImmutableHashMap();
        System.out.println("Running benchmark...");
        double juh = benchmark_j_u_HashMap();
        double ihm = benchmark_ImmutableHashMap();
        System.out.println("[java.util.HashMap] 1M perfect hash inserts across " + ITERATIONS + " iterations: " + formatDuration(juh));
        System.out.println("[ImmutableHashMap]  1M perfect hash inserts across " + ITERATIONS + " iterations: " + formatDuration(ihm));
    }

    private static String formatDuration(double ns) {
        long us = 1000;
        long ms = us * 1000;
        long s  = ms * 1000;

        return join(", ", map(into((label, div) -> (ns / div) + " " + label),
                              asList(tuple("s", s),
                                     tuple("ms", ms),
                                     tuple("us", us),
                                     tuple("ns", 1L))));

    }
}
