package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.api.MultiSet;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Natural.NonZero;
import dev.marksman.gauntlet.Arbitrary;
import dev.marksman.gauntlet.GauntletApiBase;
import dev.marksman.gauntlet.Prop;
import org.junit.Test;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Flatten.flatten;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Replicate.replicate;
import static com.jnape.palatable.lambda.functions.builtin.fn2.ToCollection.toCollection;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.functions.builtin.fn3.Times.times;
import static com.jnape.palatable.lambda.optics.Iso.simpleIso;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.atLeastOne;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.impl.HashMultiSet.hashMultiSet;
import static com.jnape.palatable.shoki.impl.TreeMultiSet.treeMultiSet;
import static com.jnape.palatable.shoki.interop.Shoki.hashSet;
import static dev.marksman.gauntlet.Arbitraries.boxedPrimitives;
import static dev.marksman.gauntlet.Arbitraries.intNaturals;
import static dev.marksman.gauntlet.Arbitraries.ints;
import static dev.marksman.gauntlet.Prop.allOf;
import static dev.marksman.gauntlet.Prop.predicate;
import static dev.marksman.kraftwerk.constraints.IntRange.inclusive;

public class MultiSetTests extends GauntletApiBase {

    @Test
    public void properties() {
        checkProperties(hashMultiSet(), boxedPrimitives());
        checkProperties(treeMultiSet(), ints());
    }

    private <A> void checkProperties(MultiSet<A> multiSet, Arbitrary<A> elements) {
        Prop<A> isEmpty  = predicate("!xs.inc(x).isEmpty()", x -> !multiSet.inc(x).isEmpty());
        Prop<A> contains = predicate("xs.inc(x).contains(x)", x -> multiSet.inc(x).contains(x));
        Prop<A> incDec   = predicate("xs.inc(x).dec(x) == xs", x -> multiSet.inc(x).dec(x).equals(multiSet));
        Prop<Tuple2<A, NonZero>> incTimes = predicate(
                "times(k.intValue(), m -> m.inc(x), xs).get(x) == k",
                into((x, k) -> times(k.intValue(), xs -> xs.inc(x), multiSet).get(x).equals(k)));
        Prop<Tuple2<A, NonZero>> incCount = predicate(
                "times(k.intValue(), m -> m.inc(x), xs) == m.inc(x, k)",
                into((x, k) -> times(k.intValue(), xs -> xs.inc(x), multiSet).equals(multiSet.inc(x, k))));
        Prop<Tuple2<A, NonZero>> remove = predicate(
                "xs.inc(x, atLeastOne(k)).remove(x) == xs",
                into((x, k) -> multiSet.inc(x, k).remove(x).equals(multiSet)));
        Prop<java.util.HashSet<A>> unique = predicate(
                "foldLeft((ms, x) -> ms.inc(x, atLeastOne(10)), multiSet, xs).unique() == hashSet(xs)",
                as -> equivalent(sameElements(),
                                 foldLeft((ms, x) -> ms.inc(x, atLeastOne(10)), multiSet, as).unique(),
                                 hashSet(as)));

        Prop<MultiSet<A>> difference = allOf(
                predicate("xs.difference(xs).isEmpty()", xs -> xs.difference(xs).isEmpty()),
                predicate("xs.difference(xs.difference(xs)) == xs", xs -> xs.difference(xs.difference(xs)).equals(xs))
        );

        Prop<MultiSet<A>> union = predicate("xs.union(xs) == xs", xs -> xs.union(xs).equals(xs));

        checkThat(all(elements).satisfy(allOf(isEmpty, contains, incDec)));
        checkThat(all(elements, positiveNaturals(atLeastOne(100))).satisfy(allOf(incTimes, incCount, remove)));
        checkThat(all(elements.hashSet()).satisfy(unique));
        checkThat(all(multiSets(multiSet, elements)).satisfy(allOf(difference, union)));
    }

    public static Natural fromBits(long bits, long... more) {
        long[] longs = Arrays.copyOfRange(more, 0, more.length + 1);
        longs[0] = bits;
        return fromBitSet(BitSet.valueOf(longs));
    }

    public static Natural fromBitSet(BitSet bits) {
        int    ln    = bits.length();
        byte[] bytes = new byte[(ln / 8) + 1];
        for (int i = 0; i < ln; i++) {
            bytes[bytes.length - i / 8 - 1] |= bits.get(i) ? 1 << (i % 8) : 0;
        }
        BigInteger value = new BigInteger(bytes);
        if (value.signum() < 0)
            throw new AssertionError("value was negative somehow");
        return abs(value);
    }

    private static Arbitrary<Natural> naturals() {
        return intNaturals().convert(simpleIso(Natural::abs, Natural::intValue));
    }

    private static Arbitrary<Natural> naturals(Natural upperBound) {
        return ints(inclusive(0, upperBound.intValue())).convert(simpleIso(Natural::abs, Natural::intValue));
    }

    private static Arbitrary<NonZero> positiveNaturals() {
        return naturals().convert(Natural::inc, nz -> nz);
    }

    private static Arbitrary<NonZero> positiveNaturals(NonZero upperBound) {
        return naturals(upperBound).convert(Natural::inc, nz -> nz);
    }

    private static <A> Arbitrary<MultiSet<A>> multiSets(MultiSet<A> ms, Arbitrary<A> elements) {
        return elements.arrayList().convert(
                as -> foldLeft(MultiSet::inc, ms, as),
                as -> toCollection(ArrayList::new, flatten(map(into((a, nz) -> replicate(nz.intValue(), a)), as))));
    }
}
