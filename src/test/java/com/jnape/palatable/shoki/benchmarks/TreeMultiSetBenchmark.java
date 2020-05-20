package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.impl.TreeMultiSet;
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
import static com.jnape.palatable.shoki.impl.TreeMultiSet.treeMultiSet;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

public class TreeMultiSetBenchmark {

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
        public TreeMultiSet<Integer> inc() {
            TreeMultiSet<Integer> treeMultiSet = treeMultiSet();
            for (int i = 0; i < K100; i++) {
                treeMultiSet = treeMultiSet.inc(i);
            }
            return treeMultiSet;
        }

        @Benchmark
        public TreeMultiSet<Integer> dec(State state) {
            TreeMultiSet<Integer> treeMultiSet = state.treeMultiSet;
            for (int i = 0; i < K100; i++) {
                treeMultiSet = treeMultiSet.dec(i);
            }
            return treeMultiSet;
        }

        @Benchmark
        public void iteration(State state, Blackhole bh) {
            state.treeMultiSet.forEach(bh::consume);
        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(TreeMultiSetBenchmark.Shoki.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Benchmark)
        public static class State {
            TreeMultiSet<Integer> treeMultiSet;

            @Setup(Level.Trial)
            public void doSetup() {
                treeMultiSet = treeMultiSet();
                for (int i = 0; i < K100; i++) {
                    treeMultiSet = treeMultiSet.inc(i);
                }
            }
        }
    }
}