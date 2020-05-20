package com.jnape.palatable.shoki.benchmarks;

import org.openjdk.jmh.runner.RunnerException;

public final class AllShokiBenchmarks {
    private AllShokiBenchmarks() {
    }

    public static void main(String[] args) throws RunnerException {
        NaturalBenchmark.main(args);
        StrictStackBenchmark.Shoki.main(args);
        StrictQueueBenchmark.Shoki.main(args);
        HashMapBenchmark.Shoki.main(args);
        HashSetBenchmark.Shoki.main(args);
        HashMultiSetBenchmark.Shoki.main(args);
        TreeMapBenchmark.Shoki.main(args);
        TreeSetBenchmark.Shoki.main(args);
        TreeMultiSetBenchmark.Shoki.main(args);
    }
}
