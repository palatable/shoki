package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.api.Queue;
import com.jnape.palatable.shoki.impl.StrictQueue;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.OperationsPerInvocation;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.util.ArrayList;
import java.util.Deque;

import static com.jnape.palatable.shoki.benchmarks.AllBenchmarks.K100;
import static com.jnape.palatable.shoki.benchmarks.QueueOps.snocRangeJDK;
import static com.jnape.palatable.shoki.benchmarks.QueueOps.snocRangeShoki;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

@BenchmarkMode(Throughput)
@OutputTimeUnit(MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(5)
@OperationsPerInvocation(K100)
public class StrictQueueBench {

    private static final int K100 = 100_000;

    @Benchmark
    public ArrayList<Integer> javaArrayList() {
        return snocRangeJDK(new ArrayList<>(), K100);
    }

    @Benchmark
    public Deque<Integer> javaLinkedList() {
        return snocRangeJDK(new java.util.LinkedList<>(), K100);
    }

    @Benchmark
    public Deque<Integer> javaArrayDeque() {
        return snocRangeJDK(new java.util.ArrayDeque<>(), K100);
    }

    @Benchmark
    public Queue<?, Integer> shokiStrictStack() {
        return snocRangeShoki(StrictQueue.empty(), K100);
    }

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(StrictQueueBench.class.getSimpleName())
                .threads(Runtime.getRuntime().availableProcessors())
                .build();

        new Runner(opt).run();
    }
}