package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.impl.HashMultiSet;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.OperationsPerInvocation;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.runner.RunnerException;

import static com.jnape.palatable.shoki.benchmarks.Benchmark.K100;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.runBenchmarks;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

public class HashMultiSetBenchmark {

    public static void main(String[] args) throws RunnerException {
        Shoki.main(args);
    }

    @BenchmarkMode(Throughput)
    @OutputTimeUnit(MICROSECONDS)
    @Warmup(iterations = 5, time = 1)
    @Measurement(iterations = 5, time = 1)
    @Fork(5)
    @OperationsPerInvocation(K100)
    public static class Shoki {

        @Benchmark
        public HashMultiSet<Integer> inc() {
            HashMultiSet<Integer> hashMultiSet = HashMultiSet.empty();
            for (int i = 0; i < K100; i++) {
                hashMultiSet = hashMultiSet.inc(i);
            }
            return hashMultiSet;
        }

        @Benchmark
        public HashMultiSet<Integer> dec(State state) {
            HashMultiSet<Integer> hashMultiSet = state.hashMultiSet;
            for (int i = 0; i < K100; i++) {
                hashMultiSet = hashMultiSet.dec(i);
            }
            return hashMultiSet;
        }

        @Benchmark
        public void iteration(State state, Blackhole bh) {
            state.hashMultiSet.forEach(bh::consume);
        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(HashMultiSetBenchmark.Shoki.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Benchmark)
        public static class State {
            HashMultiSet<Integer> hashMultiSet;

            @Setup(Level.Trial)
            public void doSetup() {
                hashMultiSet = HashMultiSet.empty();
                for (int i = 0; i < K100; i++) {
                    hashMultiSet = hashMultiSet.inc(i);
                }
            }
        }
    }
}