package com.jnape.palatable.shoki.benchmarks;

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
import org.openjdk.jmh.runner.RunnerException;

import static com.jnape.palatable.shoki.benchmarks.Benchmark.K100;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.runBenchmarks;
import static com.jnape.palatable.shoki.benchmarks.StackOps.consRangeJDK;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
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

//        @Benchmark
//        public Stack<?, Integer> cons() {
//            return consRangeShoki(strictStack(), K100);
//        }

//        @Benchmark
//        public void head(State state, Blackhole bh) {
//            for (int i = 0; i < K100; i++) {
//                bh.consume(state.strictStack.head());
//            }
//        }

        @Benchmark
        public StrictStack<Integer> tail(State state) {
            StrictStack<Integer> stack = state.strictStack;
            for (int i = 0; i < K100; i++) {
                stack = stack.tail();
            }
            return stack;
        }

//        @Benchmark
//        public void iteration(State state, Blackhole bh) {
//            state.strictStack.forEach(bh::consume);
//        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(StrictStackBenchmark.Shoki.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Thread)
        public static class State {
            StrictStack<Integer> strictStack;

            @Setup(Level.Invocation)
            public void doSetup() {
                strictStack = strictStack();
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

            @Benchmark
            public void iteration(State javaState, Blackhole bh) {
                javaState.arrayList.forEach(bh::consume);
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(StrictStackBenchmark.Java.ArrayList.class);
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

            @Benchmark
            public void iteration(State javaState, Blackhole bh) {
                javaState.linkedList.forEach(bh::consume);
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(StrictStackBenchmark.Java.LinkedList.class);
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

            @Benchmark
            public void iteration(State javaState, Blackhole bh) {
                javaState.arrayDeque.forEach(bh::consume);
            }

            public static void main(String[] args) throws RunnerException {
                runBenchmarks(StrictStackBenchmark.Java.ArrayDeque.class);
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