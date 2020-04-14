package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.impl.HashMultiSet;
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
public class HashMultiSetBench {

    private static final int K100 = 100_000;

    @Benchmark
    public HashMultiSet<Integer> shokiHashMultiSet() {
        HashMultiSet<Integer> hm = HashMultiSet.empty();
        for (int i = 0; i < K100; i++) {
            hm = hm.inc(i);
        }
        return hm;
    }

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(HashMultiSetBench.class.getSimpleName())
                .threads(Runtime.getRuntime().availableProcessors())
                .build();

        new Runner(opt).run();
    }
}