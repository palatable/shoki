package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.api.Natural;
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

import java.math.BigInteger;
import java.util.Random;

import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.benchmarks.Benchmark.runBenchmarks;
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
    public Natural randomIntPlusRandomInt(State state) {
        return state.randomInt.plus(state.randomInt);
    }

    @Benchmark
    public Natural randomIntPlusRandomLong(State state) {
        return state.randomInt.plus(state.randomLong);
    }

    @Benchmark
    public Natural randomIntPlusRandomBigInteger(State state) {
        return state.randomInt.plus(state.randomBigInteger);
    }

    @Benchmark
    public Natural randomLongPlusRandomInt(State state) {
        return state.randomLong.plus(state.randomInt);
    }

    @Benchmark
    public Natural randomLongPlusRandomLong(State state) {
        return state.randomLong.plus(state.randomLong);
    }

    @Benchmark
    public Natural randomLongPlusRandomBigInteger(State state) {
        return state.randomLong.plus(state.randomBigInteger);
    }

    @Benchmark
    public Natural randomBigIntegerPlusRandomInt(State state) {
        return state.randomBigInteger.plus(state.randomInt);
    }

    @Benchmark
    public Natural randomBigIntegerPlusRandomLong(State state) {
        return state.randomBigInteger.plus(state.randomLong);
    }

    @Benchmark
    public Natural randomBigIntegerPlusRandomBigInteger(State state) {
        return state.randomBigInteger.plus(state.randomBigInteger);
    }

    public static void main(String[] args) throws RunnerException {
        runBenchmarks(NaturalBenchmark.class);
    }

    @org.openjdk.jmh.annotations.State(Scope.Benchmark)
    public static class State {
        Natural randomInt;
        Natural randomLong;
        Natural randomBigInteger;

        @Setup(Level.Trial)
        public void doSetup() {
            Random random = new Random();
            randomInt        = abs(random.nextInt());
            randomLong       = abs(random.nextLong());
            randomBigInteger = abs(Long.MAX_VALUE).plus(abs(BigInteger.valueOf(random.nextLong())));
        }
    }
}
