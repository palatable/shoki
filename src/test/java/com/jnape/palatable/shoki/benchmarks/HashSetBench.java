package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.impl.HashSet;
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

import static com.jnape.palatable.shoki.benchmarks.AllBenchmarks.K100;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

@BenchmarkMode(Throughput)
@OutputTimeUnit(MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(5)
@OperationsPerInvocation(K100)
public class HashSetBench {

    @Benchmark
    public java.util.HashSet<Integer> javaHashSet() {
        java.util.HashSet<Integer> hm = new java.util.HashSet<>();
        for (int i = 0; i < K100; i++) {
            hm.add(i);
        }
        return hm;
    }

    @Benchmark
    public java.util.LinkedHashSet<Integer> javaLinkedHashSet() {
        java.util.LinkedHashSet<Integer> hm = new java.util.LinkedHashSet<>();
        for (int i = 0; i < K100; i++) {
            hm.add(i);
        }
        return hm;
    }

    @Benchmark
    public java.util.TreeSet<Integer> javaTreeSet() {
        java.util.TreeSet<Integer> hm = new java.util.TreeSet<>();
        for (int i = 0; i < K100; i++) {
            hm.add(i);
        }
        return hm;
    }

    @Benchmark
    public HashSet<Integer> shokiHashSet() {
        HashSet<Integer> hm = HashSet.empty();
        for (int i = 0; i < K100; i++) {
            hm = hm.add(i);
        }
        return hm;
    }

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(HashSetBench.class.getSimpleName())
                .threads(Runtime.getRuntime().availableProcessors())
                .build();

        new Runner(opt).run();
    }
}