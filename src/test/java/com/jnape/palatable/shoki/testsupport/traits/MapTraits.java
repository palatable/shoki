package com.jnape.palatable.shoki.testsupport.traits;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.lambda.io.IO;
import com.jnape.palatable.lambda.monoid.builtin.Present;
import com.jnape.palatable.shoki.api.Map;
import com.jnape.palatable.traitor.traits.Trait;

import java.util.List;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Eq.eq;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.Set.EquivalenceRelations.sameElements;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;
import static java.util.Arrays.asList;

public class MapTraits implements Trait<Map<?, Object, Object>> {

    @Override
    public void test(Map<?, Object, Object> subject) {
        List<Fn1<Map<?, Object, Object>, Maybe<String>>> assertions = asList(
                MapTraits::getMissingKey,
                MapTraits::getPresentKey,
                MapTraits::putReplacementKey,
                MapTraits::removeKey,
                MapTraits::immutability,
                MapTraits::headEmpty,
                MapTraits::headNonEmpty,
                MapTraits::tailEmpty,
                MapTraits::tailNonEmpty,
                MapTraits::keysEmpty,
                MapTraits::keysNonEmpty

        );
        Present.<String>present((x, y) -> x + "\n\t -" + y)
                .foldMap(f -> f.apply(subject), assertions)
                .match(IO::io, message -> IO.throwing(new AssertionError(
                        "One or more of the following map traits failed\n\t - " + message)))
                .unsafePerformIO();
    }

    private static Maybe<String> getMissingKey(Map<?, Object, Object> subject) {
        return subject.get(new Object()).fmap(constantly("get(missing) /= nothing()"));
    }

    private static Maybe<String> getPresentKey(Map<?, Object, Object> subject) {
        Object                 k1      = new Object();
        Object                 v1      = new Object();
        Object                 k2      = new Object();
        Object                 v2      = new Object();
        Map<?, Object, Object> updated = subject.put(k1, v1).put(k2, v2);
        return updated.get(k1).filter(eq(v1))
                .discardL(updated.get(k2))
                .fmap(constantly(Maybe.<String>nothing()))
                .orElse(just("put(k, v).get(k) /= just(v)"));
    }

    private static Maybe<String> putReplacementKey(Map<?, Object, Object> subject) {
        Object k  = new Object();
        Object v1 = new Object();
        Object v2 = new Object();
        return subject.put(k, v1).put(k, v2).get(k).filter(eq(v2))
                .fmap(constantly(Maybe.<String>nothing()))
                .orElse(just("put(k, v1).put(k, v2).get(k) /= just(v2)"));
    }

    private static Maybe<String> removeKey(Map<?, Object, Object> subject) {
        Object k1 = new Object();
        Object k2 = new Object();
        Object v1 = new Object();
        Object v2 = new Object();

        Map<?, Object, Object> postRemove = subject.put(k1, v1).put(k2, v2).remove(k1);
        return postRemove.get(k1)
                .fmap(constantly(just("remove(k).get(k) /= nothing")))
                .orElse(postRemove.get(k2).fmap(constantly(Maybe.<String>nothing()))
                                .orElse(just("put(k1, v1).put(k2, v2).remove(k1).get(k2) /= just(v2)")));
    }

    private static Maybe<String> immutability(Map<?, Object, Object> subject) {
        Object k = new Object();
        subject.put(k, new Object());
        return subject.get(k).fmap(constantly("put(k, v); get(k) /= nothing"));
    }

    private static Maybe<String> headEmpty(Map<?, Object, Object> subject) {
        return subject.head().fmap(constantly("empty.head() /= nothing"));
    }

    private static Maybe<String> headNonEmpty(Map<?, Object, Object> subject) {
        Object k = new Object();
        Object v = new Object();
        return subject.put(k, v).head().filter(eq(tuple(k, v)))
                .fmap(constantly(Maybe.<String>nothing()))
                .orElse(just("put(k, v).head() /= just(tuple(k, v))"));
    }

    private static Maybe<String> tailEmpty(Map<?, Object, Object> subject) {
        return subject.tail().equals(subject) ? nothing() : just("m.tail() /= m");
    }

    private static Maybe<String> tailNonEmpty(Map<?, Object, Object> subject) {
        Object k1 = new Object();
        Object v1 = new Object();
        return subject.put(k1, v1).tail().equals(subject) ? nothing() : just("m.put(k, v).tail() /= m");
    }

    private static Maybe<String> keysEmpty(Map<?, Object, Object> subject) {
        return subject.keys().isEmpty()
               ? nothing()
               : just("empty.keys().isEmpty() /= true");
    }

    private static Maybe<String> keysNonEmpty(Map<?, Object, Object> subject) {
        Object k1 = new Object();
        Object k2 = new Object();
        Object v1 = new Object();
        Object v2 = new Object();
        return equivalent(sameElements(), hashSet(k1, k2), subject.put(k1, v1).put(k2, v2).keys())
               ? nothing()
               : just("put(k1, v1).put(k2, v2).keys() /= [k1, k2]");
    }
}
