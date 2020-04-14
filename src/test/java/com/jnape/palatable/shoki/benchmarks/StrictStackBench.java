package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.api.Stack;
import com.jnape.palatable.shoki.impl.StrictStack;
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

import static com.jnape.palatable.shoki.benchmarks.AllBenchmarks.K100;
import static com.jnape.palatable.shoki.benchmarks.StackOps.consRangeJDK;
import static com.jnape.palatable.shoki.benchmarks.StackOps.consRangeShoki;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

public class StrictStackBench {

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(StrictStackBench.class.getSimpleName())
                .threads(Runtime.getRuntime().availableProcessors())
                .build();

        new Runner(opt).run();
    }

    @BenchmarkMode(Throughput)
    @OutputTimeUnit(MICROSECONDS)
    @Warmup(iterations = 5, time = 1)
    @Measurement(iterations = 5, time = 1)
    @Fork(5)
    @OperationsPerInvocation(K100)
    public static class Cons {

        @Benchmark
        public java.util.ArrayList<Integer> javaArrayList() {
            return consRangeJDK(new java.util.ArrayList<>(), K100);
        }

        @Benchmark
        public java.util.Deque<Integer> javaLinkedList() {
            return consRangeJDK(new java.util.LinkedList<>(), K100);
        }

        @Benchmark
        public java.util.Deque<Integer> javaArrayDeque() {
            return consRangeJDK(new java.util.ArrayDeque<>(), K100);
        }

        @Benchmark
        public Stack<?, Integer> shokiStrictStack() {
            return consRangeShoki(StrictStack.empty(), K100);
        }
    }

    @BenchmarkMode(Throughput)
    @OutputTimeUnit(MICROSECONDS)
    @Warmup(iterations = 5, time = 1)
    @Measurement(iterations = 5, time = 1)
    @Fork(5)
    @OperationsPerInvocation(K100)
    public static class HeadAndTail {

        @Benchmark
        public void javaArrayList(JavaState javaState, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(javaState.arrayList.get(0));
            }
        }

        @Benchmark
        public void javaLinkedList(JavaState javaState, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(javaState.linkedList.peek());
            }
        }

        @Benchmark
        public void javaArrayDeque(JavaState javaState, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(javaState.arrayDeque.peek());
            }
        }

        @Benchmark
        public void shokiStrictStack(ShokiState shokiState, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(shokiState.strictStack.head());
            }
        }

        @State(Scope.Benchmark)
        public static class JavaState {
            java.util.ArrayList<Integer>  arrayList  = new java.util.ArrayList<>();
            java.util.LinkedList<Integer> linkedList = new java.util.LinkedList<>();
            java.util.ArrayDeque<Integer> arrayDeque = new java.util.ArrayDeque<>();

            @Setup(Level.Trial)
            public void doSetup() {
                for (int i = 0; i < K100; i++) {
                    arrayList.add(i);
                    linkedList.addFirst(i);
                    arrayDeque.addFirst(i);
                }
            }
        }

        @State(Scope.Benchmark)
        public static class ShokiState {
            StrictStack<Integer> strictStack = StrictStack.empty();

            @Setup(Level.Trial)
            public void doSetup() {
                for (int i = 0; i < K100; i++) {
                    strictStack = strictStack.cons(i);
                }
            }
        }
    }
}