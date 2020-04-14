package com.jnape.palatable.shoki.benchmarks;

import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

public final class AllBenchmarks {

    static final int K100 = 100_000;

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include("Bench*")
                .threads(Runtime.getRuntime().availableProcessors())
                .resultFormat(ResultFormatType.JSON)
                .result("shoki.jmh.json")
                .build();

        new Runner(opt).run();
    }
}
