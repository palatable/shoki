package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.shoki.impl.HashMap;
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

@BenchmarkMode(Throughput)
@OutputTimeUnit(MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(5)
@OperationsPerInvocation(K100)
public class HashMapBenchmark {

    private static final int N_COLLISIONS = 100;

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
        public HashMap<Integer, Unit> putNoCollisions() {
            HashMap<Integer, Unit> hashMap = HashMap.hashMap();
            for (int i = 0; i < K100; i++) {
                hashMap = hashMap.put(i, UNIT);
            }
            return hashMap;
        }

        @Benchmark
        @OperationsPerInvocation(N_COLLISIONS)
        public HashMap<Collision, Unit> putFullCollisions() {
            HashMap<Collision, Unit> hashMap = HashMap.hashMap();
            for (int i = 0; i < N_COLLISIONS; i++) {
                hashMap = hashMap.put(new Collision(i), UNIT);
            }
            return hashMap;
        }

        @Benchmark
        public void getNoCollisions(NoCollisionsState state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.hashMap.get(i));
            }
        }

        @Benchmark
        @OperationsPerInvocation(N_COLLISIONS)
        public void getFullCollisions(FullCollisionsState state, Blackhole bh) {
            for (int i = 0; i < N_COLLISIONS; i++) {
                bh.consume(state.hashMap.get(new Collision(i)));
            }
        }

        @Benchmark
        public void iteration(NoCollisionsState state, Blackhole bh) {
            state.hashMap.forEach(bh::consume);
        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(HashMapBenchmark.Shoki.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class NoCollisionsState {
            HashMap<Integer, Unit> hashMap;

            @Setup(Level.Invocation)
            public void doSetup() {
                hashMap = HashMap.hashMap();
                for (int i = 0; i < K100; i++) {
                    hashMap = hashMap.put(i, UNIT);
                }
            }
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class FullCollisionsState {
            HashMap<Collision, Unit> hashMap;

            @Setup(Level.Invocation)
            public void doSetup() {
                hashMap = HashMap.hashMap();
                for (int i = 0; i < N_COLLISIONS; i++) {
                    hashMap = hashMap.put(new Collision(i), UNIT);
                }
            }
        }
    }

    public static class Java {

        public static void main(String[] args) throws RunnerException {
            HashMap.main(args);
            LinkedHashMap.main(args);
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class NoCollisionsState {
            java.util.HashMap<Integer, Unit>       hashMap;
            java.util.LinkedHashMap<Integer, Unit> linkedHashMap;
            java.util.TreeMap<Integer, Unit>       treeMap;

            @Setup(Level.Invocation)
            public void doSetup() {
                hashMap       = new java.util.HashMap<>();
                linkedHashMap = new java.util.LinkedHashMap<>();
                treeMap       = new java.util.TreeMap<>();
                for (int i = 0; i < K100; i++) {
                    hashMap.put(i, UNIT);
                    linkedHashMap.put(i, UNIT);
                    treeMap.put(i, UNIT);
                }
            }
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class FullCollisionsState {
            java.util.HashMap<Collision, Unit>       hashMap;
            java.util.LinkedHashMap<Collision, Unit> linkedHashMap;
            java.util.TreeMap<Collision, Unit>       treeMap;

            @Setup(Level.Invocation)
            public void doSetup() {
                hashMap       = new java.util.HashMap<>();
                linkedHashMap = new java.util.LinkedHashMap<>();
                treeMap       = new java.util.TreeMap<>();
                for (int i = 0; i < N_COLLISIONS; i++) {
                    Collision collision = new Collision(i);
                    hashMap.put(collision, UNIT);
                    linkedHashMap.put(collision, UNIT);
                    treeMap.put(collision, UNIT);
                }
            }
        }

        @BenchmarkMode(Throughput)
        @OutputTimeUnit(MICROSECONDS)
        @Warmup(iterations = 5, time = 1)
        @Measurement(iterations = 5, time = 1)
        @Fork(5)
        @OperationsPerInvocation(K100)
        public static class HashMap {

            @Benchmark
            public java.util.HashMap<Integer, Unit> putNoCollisions() {
                java.util.HashMap<Integer, Unit> hashMap = new java.util.HashMap<>();
                for (int i = 0; i < K100; i++) {
                    hashMap.put(i, UNIT);
                }
                return hashMap;
            }

            @Benchmark
            @OperationsPerInvocation(N_COLLISIONS)
            public java.util.HashMap<Collision, Unit> putFullCollisions() {
                java.util.HashMap<Collision, Unit> hashMap = new java.util.HashMap<>();
                for (int i = 0; i < N_COLLISIONS; i++) {
                    hashMap.put(new Collision(i), UNIT);
                }
                return hashMap;
            }

            @Benchmark
            public void getNoCollisions(NoCollisionsState state, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(state.hashMap.get(i));
                }
            }

            @Benchmark
            @OperationsPerInvocation(N_COLLISIONS)
            public void getFullCollisions(FullCollisionsState state, Blackhole bh) {
                for (int i = 0; i < N_COLLISIONS; i++) {
                    bh.consume(state.hashMap.get(new Collision(i)));
                }
            }

            @Benchmark
            public void iteration(NoCollisionsState state, Blackhole bh) {
                state.hashMap.entrySet().forEach(bh::consume);
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(HashMapBenchmark.Java.HashMap.class);
            }
        }

        @BenchmarkMode(Throughput)
        @OutputTimeUnit(MICROSECONDS)
        @Warmup(iterations = 5, time = 1)
        @Measurement(iterations = 5, time = 1)
        @Fork(5)
        @OperationsPerInvocation(K100)
        public static class LinkedHashMap {

            @Benchmark
            public java.util.LinkedHashMap<Integer, Unit> putNoCollisions() {
                java.util.LinkedHashMap<Integer, Unit> linkedHashMap = new java.util.LinkedHashMap<>();
                for (int i = 0; i < K100; i++) {
                    linkedHashMap.put(i, UNIT);
                }
                return linkedHashMap;
            }

            @Benchmark
            @OperationsPerInvocation(N_COLLISIONS)
            public java.util.LinkedHashMap<Collision, Unit> putFullCollisions() {
                java.util.LinkedHashMap<Collision, Unit> linkedHashMap = new java.util.LinkedHashMap<>();
                for (int i = 0; i < N_COLLISIONS; i++) {
                    linkedHashMap.put(new Collision(i), UNIT);
                }
                return linkedHashMap;
            }

            @Benchmark
            public void getNoCollisions(NoCollisionsState state, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(state.linkedHashMap.get(i));
                }
            }

            @Benchmark
            @OperationsPerInvocation(N_COLLISIONS)
            public void getFullCollisions(FullCollisionsState state, Blackhole bh) {
                for (int i = 0; i < N_COLLISIONS; i++) {
                    bh.consume(state.linkedHashMap.get(new Collision(i)));
                }
            }

            @Benchmark
            public void iteration(NoCollisionsState state, Blackhole bh) {
                state.linkedHashMap.entrySet().forEach(bh::consume);
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(HashMapBenchmark.Java.LinkedHashMap.class);
            }
        }
    }

    private static class Collision implements Comparable<Collision> {
        private static final int HASH_COLLISION = 1;

        private final int value;

        Collision(int value) {
            this.value = value;
        }

        @Override
        public int compareTo(Collision o) {
            return Integer.compare(value, o.value);
        }

        @Override
        public boolean equals(Object o) {
            return o instanceof Collision && value == ((Collision) o).value;
        }

        @Override
        public int hashCode() {
            return HASH_COLLISION;
        }
    }
}