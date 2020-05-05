package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.impl.HashSet;
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
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

public class HashSetBenchmark {

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
        public HashSet<Integer> add() {
            HashSet<Integer> hashSet = hashSet();
            for (int i = 0; i < K100; i++) {
                hashSet = hashSet.add(i);
            }
            return hashSet;
        }

        @Benchmark
        public HashSet<Integer> remove(State state) {
            HashSet<Integer> hashSet = state.hashSet;
            for (int i = 0; i < K100; i++) {
                hashSet = hashSet.remove(i);
            }
            return hashSet;
        }

        @Benchmark
        public void iteration(State state, Blackhole bh) {
            state.hashSet.forEach(bh::consume);
        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(HashSetBenchmark.Shoki.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Benchmark)
        public static class State {
            HashSet<Integer> hashSet;

            @Setup(Level.Trial)
            public void doSetup() {
                hashSet = hashSet();
                for (int i = 0; i < K100; i++) {
                    hashSet = hashSet.add(i);
                }
            }
        }
    }

    public static class Java {

        public static void main(String[] args) throws RunnerException {
            HashSet.main(args);
            LinkedHashSet.main(args);
            TreeSet.main(args);
        }

        @BenchmarkMode(Throughput)
        @OutputTimeUnit(MICROSECONDS)
        @Warmup(iterations = 5, time = 1)
        @Measurement(iterations = 5, time = 1)
        @Fork(5)
        @OperationsPerInvocation(K100)
        public static class HashSet {

            @Benchmark
            public java.util.HashSet<Integer> add() {
                java.util.HashSet<Integer> hashSet = new java.util.HashSet<>();
                for (int i = 0; i < K100; i++) {
                    hashSet.add(i);
                }
                return hashSet;
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(HashSetBenchmark.Java.HashSet.class);
            }
        }

        @BenchmarkMode(Throughput)
        @OutputTimeUnit(MICROSECONDS)
        @Warmup(iterations = 5, time = 1)
        @Measurement(iterations = 5, time = 1)
        @Fork(5)
        @OperationsPerInvocation(K100)
        public static class LinkedHashSet {

            @Benchmark
            public java.util.LinkedHashSet<Integer> add() {
                java.util.LinkedHashSet<Integer> linkedHashSet = new java.util.LinkedHashSet<>();
                for (int i = 0; i < K100; i++) {
                    linkedHashSet.add(i);
                }
                return linkedHashSet;
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(HashSetBenchmark.Java.LinkedHashSet.class);
            }
        }

        @BenchmarkMode(Throughput)
        @OutputTimeUnit(MICROSECONDS)
        @Warmup(iterations = 5, time = 1)
        @Measurement(iterations = 5, time = 1)
        @Fork(5)
        @OperationsPerInvocation(K100)
        public static class TreeSet {

            @Benchmark
            public java.util.TreeSet<Integer> add() {
                java.util.TreeSet<Integer> treeSet = new java.util.TreeSet<>();
                for (int i = 0; i < K100; i++) {
                    treeSet.add(i);
                }
                return treeSet;
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(HashSetBenchmark.Java.TreeSet.class);
            }
        }
    }
}