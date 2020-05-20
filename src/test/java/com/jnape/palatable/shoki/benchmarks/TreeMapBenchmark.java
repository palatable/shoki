package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.shoki.impl.TreeMap;
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

import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.K100;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.runBenchmarks;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

public class TreeMapBenchmark {

    public static void main(String[] args) throws RunnerException {
        Shoki.main(args);
        Java.main(args);
    }

    @BenchmarkMode(Throughput)
    @OutputTimeUnit(MICROSECONDS)
    @Warmup(iterations = 5, time = 1)
    @Measurement(iterations = 5, time = 1)
    @Fork(5)
    @OperationsPerInvocation(K100)
    public static class Shoki {

        @Benchmark
        public TreeMap<Integer, Unit> put() {
            TreeMap<Integer, Unit> treeMap = TreeMap.treeMap();
            for (int i = 0; i < K100; i++) {
                treeMap = treeMap.put(i, UNIT);
            }
            return treeMap;
        }

        @Benchmark
        public void get(State state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.treeMap.get(i));
            }
        }

        @Benchmark
        public void min(State state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.treeMap.min());
            }
        }

        @Benchmark
        public void max(State state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.treeMap.max());
            }
        }

        @Benchmark
        public void iteration(State state, Blackhole bh) {
            state.treeMap.forEach(bh::consume);
        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(TreeMapBenchmark.Shoki.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class State {
            TreeMap<Integer, Unit> treeMap;

            @Setup(Level.Invocation)
            public void doSetup() {
                treeMap = TreeMap.treeMap();
                for (int i = 0; i < K100; i++) {
                    treeMap = treeMap.put(i, UNIT);
                }
            }
        }
    }

    @BenchmarkMode(Throughput)
    @OutputTimeUnit(MICROSECONDS)
    @Warmup(iterations = 5, time = 1)
    @Measurement(iterations = 5, time = 1)
    @Fork(5)
    @OperationsPerInvocation(K100)
    public static class Java {

        @Benchmark
        public java.util.TreeMap<Integer, Unit> put() {
            java.util.TreeMap<Integer, Unit> treeMap = new java.util.TreeMap<>();
            for (int i = 0; i < K100; i++) {
                treeMap.put(i, UNIT);
            }
            return treeMap;
        }

        @Benchmark
        public void get(State state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.treeMap.get(i));
            }
        }

        @Benchmark
        public void min(State state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.treeMap.firstEntry());
            }
        }

        @Benchmark
        public void max(State state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.treeMap.lastEntry());
            }
        }

        @Benchmark
        public void iteration(State state, Blackhole bh) {
            state.treeMap.entrySet().forEach(bh::consume);
        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(TreeMapBenchmark.Java.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class State {
            java.util.TreeMap<Integer, Unit> treeMap;

            @Setup(Level.Invocation)
            public void doSetup() {
                treeMap = new java.util.TreeMap<>();
                for (int i = 0; i < K100; i++) {
                    treeMap.put(i, UNIT);
                }
            }
        }
    }
}
