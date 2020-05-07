package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.impl.StrictQueue;
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
import org.openjdk.jmh.runner.RunnerException;

import java.util.Random;

import static com.jnape.palatable.lambda.functions.builtin.fn3.Times.times;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.runBenchmarks;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static org.openjdk.jmh.annotations.Mode.Throughput;

@BenchmarkMode(Throughput)
@OutputTimeUnit(MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(5)
public class NaturalBenchmark {

    @Benchmark
    @OperationsPerInvocation(1_000_000)
    public Natural inc() {
        Natural nat = zero();
        for (int i = 0; i < 1_000_000; i++) {
            nat = nat.inc();
        }
        return nat;
    }

    @Benchmark
    @OperationsPerInvocation(1_000_000)
    public Natural plusRandom(State state) {
        Natural nat = zero();
        for (Natural n : state.ints) {
            nat = nat.plus(n);
        }
        return nat;
    }

    public static void main(String[] args) throws RunnerException {
        runBenchmarks(NaturalBenchmark.class);
    }

    @org.openjdk.jmh.annotations.State(Scope.Benchmark)
    public static class State {
        StrictQueue<Natural> ints;

        @Setup(Level.Trial)
        public void doSetup() {
            Random random = new Random();
            ints = times(1_000_000, q -> q.cons(abs(random.nextInt())), strictQueue());
        }
    }
}
