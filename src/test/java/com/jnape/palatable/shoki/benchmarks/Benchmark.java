package com.jnape.palatable.shoki.benchmarks;

import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.io.File;

import static java.lang.String.join;
import static java.util.Arrays.copyOf;

public final class Benchmark {

    static final int K100 = 100_000;

    private static final String ROOT_PACKAGE_PATH = join("/", "src", "test", "java");
    private static final String RESULTS_DIR_NAME  = "results";

    public static void runBenchmarks(Class<?> benchmarkClass) throws RunnerException {
        new Runner(shokiOptions(benchmarkClass)).run();
    }

    private static Options shokiOptions(Class<?> benchmarkClass) {
        return new OptionsBuilder()
                .include(benchmarkClass.getCanonicalName())
                .threads(Runtime.getRuntime().availableProcessors())
                .resultFormat(ResultFormatType.JSON)
                .result(resultsFilePath(benchmarkClass))
                .build();
    }

    private static String resultsFilePath(Class<?> benchmarkClass) {
        String   packageName          = benchmarkClass.getPackage().getName();
        String[] nestedClassesAndName = benchmarkClass.getCanonicalName().substring(packageName.length() + 1).split("\\.");
        String   packagePath          = join("/", packageName.split("\\."));
        String   nestedClassPath      = join("/", copyOf(nestedClassesAndName, nestedClassesAndName.length - 1));
        String   resultsDirectoryPath = join("/", ROOT_PACKAGE_PATH, packagePath, RESULTS_DIR_NAME, nestedClassPath);
        String   fileName             = join(".", nestedClassesAndName[nestedClassesAndName.length - 1], "jmh", "json");
        File     resultsDirectory     = new File(resultsDirectoryPath);
        if (!(resultsDirectory.mkdirs() || resultsDirectory.isDirectory())) {
            throw new IllegalStateException("Failed to create JMH results directory: " + resultsDirectoryPath);
        }
        return join("/", resultsDirectoryPath, fileName);
    }
}
