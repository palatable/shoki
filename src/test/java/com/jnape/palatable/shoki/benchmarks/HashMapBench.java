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
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.util.ArrayList;

import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static com.jnape.palatable.shoki.benchmarks.AllBenchmarks.K100;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

@BenchmarkMode(Throughput)
@OutputTimeUnit(MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(5)
@OperationsPerInvocation(K100)
public class HashMapBench {

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(HashMapBench.class.getSimpleName())
                .threads(Runtime.getRuntime().availableProcessors())
                .build();

        new Runner(opt).run();
    }

    public static class Put extends HashMapBench {

        public static class NoCollisions extends Put {

            @Benchmark
            public java.util.HashMap<Integer, Unit> javaHashMap() {
                java.util.HashMap<Integer, Unit> hm = new java.util.HashMap<>();
                for (int i = 0; i < K100; i++) {
                    hm.put(i, UNIT);
                }
                return hm;
            }

            @Benchmark
            public java.util.LinkedHashMap<Integer, Unit> javaLinkedHashMap() {
                java.util.LinkedHashMap<Integer, Unit> hm = new java.util.LinkedHashMap<>();
                for (int i = 0; i < K100; i++) {
                    hm.put(i, UNIT);
                }
                return hm;
            }

            @Benchmark
            public java.util.TreeMap<Integer, Unit> javaTreeMap() {
                java.util.TreeMap<Integer, Unit> hm = new java.util.TreeMap<>();
                for (int i = 0; i < K100; i++) {
                    hm.put(i, UNIT);
                }
                return hm;
            }

            @Benchmark
            public HashMap<Integer, Unit> shokiHashMap() {
                HashMap<Integer, Unit> hm = HashMap.empty();
                for (int i = 0; i < K100; i++) {
                    hm = hm.put(i, UNIT);
                }
                return hm;
            }
        }

        @OperationsPerInvocation(FullCollisions.N)
        public static class FullCollisions extends Put {

            private static final int N = 100;

            @Benchmark
            public java.util.HashMap<Collision, Unit> javaHashMap(JavaAndShokiState state) {
                java.util.HashMap<Collision, Unit> hm = new java.util.HashMap<>();
                for (int i = 0; i < N; i++) {
                    hm.put(state.collisions.get(i), UNIT);
                }
                return hm;
            }

            @Benchmark
            public java.util.LinkedHashMap<Collision, Unit> javaLinkedHashMap(JavaAndShokiState state) {
                java.util.LinkedHashMap<Collision, Unit> hm = new java.util.LinkedHashMap<>();
                for (int i = 0; i < N; i++) {
                    hm.put(state.collisions.get(i), UNIT);
                }
                return hm;
            }

            @Benchmark
            public java.util.TreeMap<Collision, Unit> javaTreeMap(JavaAndShokiState state) {
                java.util.TreeMap<Collision, Unit> hm = new java.util.TreeMap<>();
                for (int i = 0; i < N; i++) {
                    hm.put(state.collisions.get(i), UNIT);
                }
                return hm;
            }

            @Benchmark
            public HashMap<Collision, Unit> shokiHashMap(JavaAndShokiState state) {
                HashMap<Collision, Unit> hm = HashMap.empty();
                for (int i = 0; i < N; i++) {
                    hm = hm.put(state.collisions.get(i), UNIT);
                }
                return hm;
            }

            @State(Scope.Benchmark)
            public static class JavaAndShokiState {
                ArrayList<Collision> collisions = new ArrayList<>();

                @Setup(Level.Trial)
                public void doSetup() {
                    for (int i = 0; i < N; i++) {
                        collisions.add(new Collision(i));
                    }
                }

            }
        }
    }

    public static class Get extends HashMapBench {

        public static class NoCollisions extends Get {

            @Benchmark
            public void javaHashMap(JavaState javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.hashMap.get(i));
                }
            }

            @Benchmark
            public void javaLinkedHashMap(JavaState javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.linkedHashMap.get(i));
                }
            }

            @Benchmark
            public void javaTreeMap(JavaState javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.treeMap.get(i));
                }
            }

            @Benchmark
            public void shokiHashMap(ShokiState shokiState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(shokiState.hashMap.get(i));
                }
            }

            @State(Scope.Benchmark)
            public static class JavaState {
                java.util.HashMap<Integer, Unit>       hashMap       = new java.util.HashMap<>();
                java.util.LinkedHashMap<Integer, Unit> linkedHashMap = new java.util.LinkedHashMap<>();
                java.util.TreeMap<Integer, Unit>       treeMap       = new java.util.TreeMap<>();

                @Setup(Level.Trial)
                public void doSetup() {
                    for (int i = 0; i < K100; i++) {
                        hashMap.put(i, UNIT);
                        linkedHashMap.put(i, UNIT);
                        treeMap.put(i, UNIT);
                    }
                }
            }

            @State(Scope.Benchmark)
            public static class ShokiState {
                HashMap<Integer, Unit> hashMap = HashMap.empty();

                @Setup(Level.Trial)
                public void doSetup() {
                    for (int i = 0; i < K100; i++) {
                        hashMap = hashMap.put(i, UNIT);
                    }
                }
            }
        }

        @OperationsPerInvocation(FullCollisions.N)
        public static class FullCollisions extends Get {

            private static final int N = 100;

            @Benchmark
            public void javaHashMap(JavaState javaState, Blackhole bh) {
                for (int i = 0; i < N; i++) {
                    bh.consume(javaState.hashMap.get(new Collision(i)));
                }
            }

            @Benchmark
            public void javaLinkedHashMap(JavaState javaState, Blackhole bh) {
                for (int i = 0; i < N; i++) {
                    bh.consume(javaState.linkedHashMap.get(new Collision(i)));
                }
            }

            @Benchmark
            public void javaTreeMap(JavaState javaState, Blackhole bh) {
                for (int i = 0; i < N; i++) {
                    bh.consume(javaState.treeMap.get(new Collision(i)));
                }
            }

            @Benchmark
            public void shokiHashMap(ShokiState shokiState, Blackhole bh) {
                for (int i = 0; i < N; i++) {
                    bh.consume(shokiState.hashMap.get(new Collision(i)));
                }
            }

            @State(Scope.Benchmark)
            public static class JavaState {
                java.util.HashMap<Collision, Unit>       hashMap       = new java.util.HashMap<>();
                java.util.LinkedHashMap<Collision, Unit> linkedHashMap = new java.util.LinkedHashMap<>();
                java.util.TreeMap<Collision, Unit>       treeMap       = new java.util.TreeMap<>();

                @Setup(Level.Trial)
                public void doSetup() {
                    for (int i = 0; i < N; i++) {
                        Collision collision = new Collision(i);
                        hashMap.put(collision, UNIT);
                        linkedHashMap.put(collision, UNIT);
                        treeMap.put(collision, UNIT);
                    }
                }
            }

            @State(Scope.Benchmark)
            public static class ShokiState {
                HashMap<Collision, Unit> hashMap = HashMap.empty();

                @Setup(Level.Trial)
                public void doSetup() {
                    for (int i = 0; i < N; i++) {
                        hashMap = hashMap.put(new Collision(i), UNIT);
                    }
                }
            }
        }
    }

    public static class Iteration extends HashMapBench {

        @Benchmark
        public void javaHashMap(JavaState javaState, Blackhole bh) {
            javaState.hashMap.entrySet().forEach(bh::consume);
        }

        @Benchmark
        public void javaLinkedHashMap(JavaState javaState, Blackhole bh) {
            javaState.linkedHashMap.entrySet().forEach(bh::consume);
        }

        @Benchmark
        public void javaTreeMap(JavaState javaState, Blackhole bh) {
            javaState.treeMap.entrySet().forEach(bh::consume);
        }

        @Benchmark
        public void shokiHashMap(ShokiState shokiState, Blackhole bh) {
            shokiState.hashMap.forEach(bh::consume);
        }

        @State(Scope.Benchmark)
        public static class JavaState {
            java.util.HashMap<Integer, Unit>       hashMap       = new java.util.HashMap<>();
            java.util.LinkedHashMap<Integer, Unit> linkedHashMap = new java.util.LinkedHashMap<>();
            java.util.TreeMap<Integer, Unit>       treeMap       = new java.util.TreeMap<>();

            @Setup(Level.Trial)
            public void doSetup() {
                for (int i = 0; i < K100; i++) {
                    hashMap.put(i, UNIT);
                    linkedHashMap.put(i, UNIT);
                    treeMap.put(i, UNIT);
                }
            }
        }

        @State(Scope.Benchmark)
        public static class ShokiState {
            HashMap<Integer, Unit> hashMap = HashMap.empty();

            @Setup(Level.Trial)
            public void doSetup() {
                for (int i = 0; i < K100; i++) {
                    hashMap = hashMap.put(i, UNIT);
                }
            }
        }
    }

    static class Collision implements Comparable<Collision> {
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