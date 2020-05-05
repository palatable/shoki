package com.jnape.palatable.shoki.interop;

import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;

import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;

import static com.jnape.palatable.lambda.functions.Fn2.curried;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn2.GTE.gte;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Iterate.iterate;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn2.TakeWhile.takeWhile;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.Natural.atLeastZero;

/**
 * Common interoperability methods for translating from built-in Java types to Shoki types.
 */
public final class Shoki {
    private Shoki() {
    }

    /**
     * Construct a {@link StrictStack} from an input {@link java.util.Collection}, preserving the same ordering of
     * elements.
     * <p>
     * Due to the {@link StrictStack#cons(Object) lifo} nature of {@link StrictStack}, this method makes an effort to
     * iterate the elements of the input {@link java.util.Collection collection} in reverse, if a known type
     * representing that capability has been advertised by the {@link java.util.Collection collection}. If no reverse
     * iteration strategy can be deduced from the input {@link java.util.Collection collection}, the elements will be
     * {@link StrictStack#cons(Object) cons'ed} onto the {@link StrictStack} in one pass and the {@link StrictStack}
     * will be {@link StrictStack#reverse() reversed} before being returned.
     *
     * @param javaCollection the input {@link java.util.Collection}
     * @param <A>            the element type
     * @return the populated {@link StrictStack}
     */
    public static <A> StrictStack<A> strictStack(java.util.Collection<A> javaCollection) {
        return javaCollection instanceof java.util.Deque<?>
               ? foldLeft(StrictStack::cons, StrictStack.empty(),
                          ((java.util.Deque<A>) javaCollection)::descendingIterator)
               : javaCollection instanceof java.util.List<?>
                 ? foldLeft(StrictStack::cons, StrictStack.empty(),
                            javaCollection instanceof java.util.RandomAccess
                            ? map(((java.util.List<A>) javaCollection)::get,
                                  takeWhile(gte(0), iterate(i -> i - 1, javaCollection.size() - 1)))
                            : () -> {
                                ListIterator<A> itr = ((List<A>) javaCollection).listIterator(javaCollection.size());
                                return new Iterator<A>() {
                                    @Override
                                    public boolean hasNext() {
                                        return itr.hasPrevious();
                                    }

                                    @Override
                                    public A next() {
                                        return itr.previous();
                                    }
                                };
                            })
                 : foldLeft(StrictStack::cons, StrictStack.<A>empty(), javaCollection).reverse();
    }

    /**
     * Construct a {@link StrictQueue} from an input {@link java.util.Collection}, preserving the same ordering of
     * elements.
     *
     * @param javaCollection the input {@link java.util.Collection}
     * @param <A>            the element type
     * @return the populated {@link StrictQueue}
     */
    public static <A> StrictQueue<A> strictQueue(java.util.Collection<A> javaCollection) {
        return foldLeft(StrictQueue::snoc, StrictQueue.empty(), javaCollection);
    }

    /**
     * Construct a {@link HashMap} from an input {@link java.util.Map}, using
     * {@link Objects#equals(Object, Object) Object equality} and {@link Objects#hashCode(Object) Object hashCode} as
     * the {@link EquivalenceRelation} and {@link HashingAlgorithm}, respectively, for its keys.
     *
     * @param javaMap the input {@link java.util.Map}
     * @param <K>     the key type
     * @param <V>     the value type
     * @return the populated {@link HashMap}
     */
    public static <K, V> HashMap<K, V> hashMap(java.util.Map<K, V> javaMap) {
        return foldLeft(curried(hm -> into(hm::put)), HashMap.empty(), javaMap.entrySet());
    }

    /**
     * Construct a {@link HashSet} from an input {@link java.util.Set}, using
     * {@link Objects#equals(Object, Object) Object equality} and {@link Objects#hashCode(Object) Object hashCode} as
     * the {@link EquivalenceRelation} and {@link HashingAlgorithm}, respectively, for its elements.
     *
     * @param javaCollection the input {@link java.util.Collection}
     * @param <A>            the element type
     * @return the populated {@link HashSet}
     */
    public static <A> HashSet<A> hashSet(java.util.Collection<A> javaCollection) {
        return foldLeft(HashSet::add, HashSet.empty(), javaCollection);
    }

    /**
     * Construct a {@link HashMultiSet} from an input {@link java.util.Map}, using
     * {@link Objects#equals(Object, Object) Object equality} and {@link Objects#hashCode(Object) Object hashCode} as
     * the {@link EquivalenceRelation} and {@link HashingAlgorithm}, respectively, for its elements.
     * <p>
     * Note that the resulting {@link HashMultiSet} will only {@link HashMultiSet#contains(Object) contain} elements
     * from the input {@link java.util.Map map} that mapped to a positive, non-zero integral value.
     *
     * @param javaMap the input {@link java.util.Map}
     * @param <A>     the element type
     * @return the populated {@link HashMultiSet}
     */
    public static <A> HashMultiSet<A> hashMultiSet(java.util.Map<A, Integer> javaMap) {
        return foldLeft(curried(hms -> into((a, k) -> atLeastZero(k).match(constantly(hms),
                                                                           nonZeroK -> hms.inc(a, nonZeroK)))),
                        HashMultiSet.empty(),
                        javaMap.entrySet());
    }

    /**
     * Construct a {@link HashMultiSet} from an input {@link java.util.Collection}, using
     * {@link Objects#equals(Object, Object) Object equality} and {@link Objects#hashCode(Object) Object hashCode} as
     * the {@link EquivalenceRelation} and {@link HashingAlgorithm}, respectively, for its elements.
     *
     * @param javaCollection the input {@link java.util.Collection}
     * @param <A>            the element type
     * @return the populated {@link HashMultiSet}
     */
    public static <A> HashMultiSet<A> hashMultiSet(java.util.Collection<A> javaCollection) {
        return foldLeft(HashMultiSet::inc, HashMultiSet.empty(), javaCollection);
    }
}
