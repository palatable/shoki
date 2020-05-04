package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.impl.PriorityHeap;
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

public class PriorityHeapBenchmark {

    public static void main(String[] args) throws RunnerException {
        HashMultiSetBenchmark.Shoki.main(args);
    }

    @BenchmarkMode(Throughput)
    @OutputTimeUnit(MICROSECONDS)
    @Warmup(iterations = 5, time = 1)
    @Measurement(iterations = 5, time = 1)
    @Fork(5)
    @OperationsPerInvocation(K100)
    public static class Shoki {

        @Benchmark
        public PriorityHeap<Integer> enqueue() {
            PriorityHeap<Integer> priorityHeap = PriorityHeap.min();
            for (int i = 0; i < K100; i++) {
                priorityHeap = priorityHeap.insert(i);
            }
            return priorityHeap;
        }

        @Benchmark
        public void head(State state, Blackhole bh) {
            PriorityHeap<Integer> priorityHeap = state.priorityHeap;
            for (int i = 0; i < K100; i++) {
                bh.consume(priorityHeap.head());
            }
        }

        public static void main(String[] args) throws RunnerException {
            runBenchmarks(PriorityHeapBenchmark.Shoki.class);
        }

        @org.openjdk.jmh.annotations.State(Scope.Benchmark)
        public static class State {
            PriorityHeap<Integer> priorityHeap;

            @Setup(Level.Trial)
            public void doSetup() {
                priorityHeap = PriorityHeap.min();
                for (int i = 0; i < K100; i++) {
                    priorityHeap = priorityHeap.insert(i);
                }
            }
        }
    }

}
