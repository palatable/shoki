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
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;

import static com.jnape.palatable.lambda.adt.choice.Choice2.b;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.K100;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.shokiOptions;
import static com.jnape.palatable.shoki.benchmarks.StackOps.consRangeJDK;
import static com.jnape.palatable.shoki.benchmarks.StackOps.consRangeShoki;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

public class StrictStackBenchmark {

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
        public Stack<?, Integer> cons() {
            return consRangeShoki(StrictStack.empty(), K100);
        }

        @Benchmark
        public void head(State state, Blackhole bh) {
            for (int i = 0; i < K100; i++) {
                bh.consume(state.strictStack.head());
            }
        }

        @Benchmark
        public StrictStack<Integer> tail(State state) {
            StrictStack<Integer> stack = state.strictStack;
            for (int i = 0; i < K100; i++) {
                stack = stack.tail();
            }
            return stack;
        }

        public static void main(String[] args) throws RunnerException {
            new Runner(shokiOptions(b(StrictStackBenchmark.Shoki.class), StrictStackBenchmark.Shoki.class)).run();
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class State {
            StrictStack<Integer> strictStack;

            @Setup(Level.Invocation)
            public void doSetup() {
                strictStack = StrictStack.empty();
                for (int i = 0; i < K100; i++) {
                    strictStack = strictStack.cons(i);
                }
            }
        }
    }

    public static class Java {

        public static void main(String[] args) throws RunnerException {
            ArrayList.main(args);
            LinkedList.main(args);
            ArrayDeque.main(args);
        }

        @BenchmarkMode(Throughput)
        @OutputTimeUnit(MICROSECONDS)
        @Warmup(iterations = 5, time = 1)
        @Measurement(iterations = 5, time = 1)
        @Fork(5)
        @OperationsPerInvocation(K100)
        public static class ArrayList {

            @Benchmark
            public java.util.ArrayList<Integer> cons() {
                return consRangeJDK(new java.util.ArrayList<>(), K100);
            }

            @Benchmark
            public void head(State javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.arrayList.get(0));
                }
            }

            @Benchmark
            public void tail(State javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.arrayList.remove(0));
                }
            }

            public static void main(String[] args) throws RunnerException {
                new Runner(shokiOptions(b(StrictStackBenchmark.Java.ArrayList.class),
                                        StrictStackBenchmark.Java.ArrayList.class)).run();
            }

            @org.openjdk.jmh.annotations.State(Scope.Thread)
            public static class State {
                java.util.ArrayList<Integer> arrayList;

                @Setup(Level.Invocation)
                public void doSetup() {
                    arrayList = new java.util.ArrayList<>();
                    for (int i = 0; i < K100; i++) {
                        arrayList.add(i);
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
        public static class LinkedList {

            @Benchmark
            public java.util.Deque<Integer> cons() {
                return consRangeJDK(new java.util.LinkedList<>(), K100);
            }

            @Benchmark
            public void head(State javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.linkedList.peekFirst());
                }
            }

            @Benchmark
            public void tail(State javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.linkedList.pop());
                }
            }

            public static void main(String[] args) throws RunnerException {
                new Runner(shokiOptions(b(StrictStackBenchmark.Java.LinkedList.class),
                                        StrictStackBenchmark.Java.LinkedList.class)).run();
            }

            @org.openjdk.jmh.annotations.State(Scope.Thread)
            public static class State {
                java.util.LinkedList<Integer> linkedList;

                @Setup(Level.Invocation)
                public void doSetup() {
                    linkedList = new java.util.LinkedList<>();
                    for (int i = 0; i < K100; i++) {
                        linkedList.addFirst(i);
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
        public static class ArrayDeque {

            @Benchmark
            public java.util.Deque<Integer> cons() {
                return consRangeJDK(new java.util.ArrayDeque<>(), K100);
            }

            @Benchmark
            public void head(State javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.arrayDeque.peekFirst());
                }
            }

            @Benchmark
            public void tail(State javaState, Blackhole bh) {
                for (int i = 0; i < K100; i++) {
                    bh.consume(javaState.arrayDeque.pop());
                }
            }

            public static void main(String[] args) throws RunnerException {
                new Runner(shokiOptions(b(StrictStackBenchmark.Java.ArrayDeque.class),
                                        StrictStackBenchmark.Java.ArrayDeque.class)).run();
            }

            @org.openjdk.jmh.annotations.State(Scope.Thread)
            public static class State {
                java.util.ArrayDeque<Integer> arrayDeque;

                @Setup(Level.Invocation)
                public void doSetup() {
                    arrayDeque = new java.util.ArrayDeque<>();
                    for (int i = 0; i < K100; i++) {
                        arrayDeque.addFirst(i);
                    }
                }
            }
        }
    }
}