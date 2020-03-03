package com.jnape.palatable.shoki.bench.jmh;

import com.jnape.palatable.lambda.adt.Unit;
import com.jnape.palatable.shoki.ImmutableHashMap;
import fj.data.hamt.HashArrayMappedTrie;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.util.HashMap;

import static com.jnape.palatable.lambda.adt.Unit.UNIT;
import static java.util.concurrent.TimeUnit.NANOSECONDS;
import static org.openjdk.jmh.annotations.Mode.AverageTime;

@BenchmarkMode(AverageTime)
@OutputTimeUnit(NANOSECONDS)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
public class ImmutableHashMapMicroBenchmark {

    public static final int INSERTS = 1_000_000;

    @Benchmark
    public HashMap<Integer, Unit> _1M_Perfect_Hash_Inserts_And_Subsequent_Reads_juHashMap(Blackhole bh) {
        HashMap<Integer, Unit> m = new HashMap<>();
        for (int i = 0; i < INSERTS; i++) {
            m.put(i, UNIT);
        }
        for (int i = 0; i < INSERTS; i++) {
            bh.consume(m.get(i));
        }
        return m;
    }

    @Benchmark
    public ImmutableHashMap<Integer, Unit> _1M_Perfect_Hash_Inserts_And_Subsequent_Reads_ImmutableHashMap(Blackhole bh) {
        ImmutableHashMap<Integer, Unit> m = ImmutableHashMap.empty();
        for (int i = 0; i < INSERTS; i++) {
            m = m.put(i, UNIT);
        }
        for (int i = 0; i < INSERTS; i++) {
            bh.consume(m.get(i));
        }
        return m;
    }

    @Benchmark
    public HashArrayMappedTrie<Integer, Unit> _1M_Perfect_Hash_Inserts_And_Subsequent_Reads_fjHashArrayMappedTrie(Blackhole bh) {
        HashArrayMappedTrie<Integer, Unit> m = HashArrayMappedTrie.emptyKeyInteger();
        for (int i = 0; i < INSERTS; i++) {
            m = m.set(i, UNIT);
        }
        for (int i = 0; i < INSERTS; i++) {
            bh.consume(m.find(i));
        }
        return m;
    }
}
