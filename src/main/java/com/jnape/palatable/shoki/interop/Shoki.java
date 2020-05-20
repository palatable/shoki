package com.jnape.palatable.shoki.interop;

import com.jnape.palatable.shoki.api.EquivalenceRelation;
import com.jnape.palatable.shoki.api.HashingAlgorithm;
import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;
import com.jnape.palatable.shoki.impl.TreeMap;
import com.jnape.palatable.shoki.impl.TreeMultiSet;
import com.jnape.palatable.shoki.impl.TreeSet;

import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;
import java.util.SortedMap;
import java.util.SortedSet;

import static com.jnape.palatable.lambda.functions.Fn2.curried;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.Natural.atLeastZero;
import static java.util.Comparator.naturalOrder;

/**
 * Common interoperability methods for translating from built-in Java types to Shoki types.
 */
public final class Shoki {
    private Shoki() {
    }

    /**
     * Construct a {@link StrictStack} from an input {@link Iterable}, preserving the same ordering of elements.
     * <p>
     * Due to the {@link StrictStack#cons(Object) lifo} nature of {@link StrictStack}, this method makes an effort to
     * iterate the elements of the input {@link Iterable} in reverse, if a known type representing that capability has
     * been advertised by the {@link Iterable}. If no reverse iteration strategy can be deduced from the input
     * {@link Iterable}, the elements will be {@link StrictStack#cons(Object) consed} onto the {@link StrictStack} in
     * one pass and the {@link StrictStack} will be {@link StrictStack#reverse() reversed} before being returned.
     *
     * @param javaIterable the input {@link Iterable}
     * @param <A>          the element type
     * @return the populated {@link StrictStack}
     */
    public static <A> StrictStack<A> strictStack(Iterable<A> javaIterable) {
        Iterator<A> bestIteratorForStackConstruction =
                javaIterable instanceof Deque<?>
                ? ((Deque<A>) javaIterable).descendingIterator()
                : javaIterable instanceof List<?>
                  ? new Iterator<A>() {
                    private final List<A> javaList = (List<A>) javaIterable;
                    private final ListIterator<A> itr = javaList.listIterator(javaList.size());

                    @Override
                    public boolean hasNext() {
                        return itr.hasPrevious();
                    }

                    @Override
                    public A next() {
                        return itr.previous();
                    }
                }
                  : foldLeft(StrictStack::cons, StrictStack.<A>strictStack(), javaIterable).iterator();

        return foldLeft(StrictStack::cons, StrictStack.strictStack(), () -> bestIteratorForStackConstruction);
    }

    /**
     * Construct a {@link StrictQueue} from an input {@link Iterable}, preserving the same ordering of elements.
     *
     * @param javaIterable the input {@link Iterable}
     * @param <A>          the element type
     * @return the populated {@link StrictQueue}
     */
    public static <A> StrictQueue<A> strictQueue(Iterable<A> javaIterable) {
        return foldLeft(StrictQueue::snoc, StrictQueue.strictQueue(), javaIterable);
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
        return foldLeft(curried(hm -> into(hm::put)), HashMap.hashMap(), javaMap.entrySet());
    }

    /**
     * Construct a {@link TreeMap} from an input {@link java.util.Map}, using the given {@link Comparator} for its key
     * comparison relation.
     *
     * @param keyComparator the {@link Comparator}
     * @param javaMap       the input {@link java.util.Map}
     * @param <K>           the key type
     * @param <V>           the value type
     * @return the populated {@link TreeMap}
     */
    public static <K, V> TreeMap<K, V> treeMap(Comparator<? super K> keyComparator,
                                               java.util.Map<K, V> javaMap) {
        return foldLeft(curried(tm -> into(tm::put)), TreeMap.treeMap(keyComparator), javaMap.entrySet());
    }

    /**
     * Construct a {@link TreeMap} from an input {@link java.util.Map} whose keys are {@link Comparable}. If the input
     * map is an instance of a {@link SortedMap} with a non-null {@link Comparator}, that {@link Comparator} will be
     * used for the key comparison relation; otherwise, {@link Comparator#naturalOrder() natural ordering} will be
     * used.
     *
     * @param javaMap the input {@link java.util.Map}
     * @param <K>     the {@link Comparable} key type
     * @param <V>     the value type
     * @return the populated {@link TreeMap}
     */
    public static <K extends Comparable<? super K>, V> TreeMap<K, V> treeMap(java.util.Map<K, V> javaMap) {
        Comparator<? super K> keyComparator = javaMap instanceof java.util.SortedMap<?, ?>
                                              ? ((java.util.SortedMap<K, V>) javaMap).comparator()
                                              : null;
        return treeMap(keyComparator == null ? naturalOrder() : keyComparator, javaMap);
    }

    /**
     * Construct a {@link HashSet} from an input {@link Iterable}, using
     * {@link Objects#equals(Object, Object) Object equality} and {@link Objects#hashCode(Object) Object hashCode} as
     * the {@link EquivalenceRelation} and {@link HashingAlgorithm}, respectively, for its elements.
     *
     * @param javaIterable the input {@link Iterable}
     * @param <A>          the element type
     * @return the populated {@link HashSet}
     */
    public static <A> HashSet<A> hashSet(Iterable<A> javaIterable) {
        return foldLeft(HashSet::add, HashSet.hashSet(), javaIterable);
    }

    /**
     * Construct a {@link TreeSet} from an input {@link Iterable}, using the given {@link Comparator} for its
     * comparison relation.
     *
     * @param comparator   the {@link Comparator}
     * @param javaIterable the input {@link Iterable}
     * @param <A>          the element type
     * @return the populated {@link TreeSet}
     */
    public static <A> TreeSet<A> treeSet(Comparator<? super A> comparator, Iterable<A> javaIterable) {
        return foldLeft(TreeSet::add, TreeSet.treeSet(comparator), javaIterable);
    }

    /**
     * Construct a {@link TreeSet} from an input {@link Iterable}. If the input iterable is an instance of a
     * {@link SortedSet} with a non-null {@link Comparator}, that {@link Comparator} will be used for the comparison
     * relation; otherwise, {@link Comparator#naturalOrder() natural ordering} will be used.
     *
     * @param javaIterable the input {@link Iterable}
     * @param <A>          the element type
     * @return the populated {@link HashSet}
     */
    public static <A extends Comparable<? super A>> TreeSet<A> treeSet(Iterable<A> javaIterable) {
        Comparator<? super A> comparator = javaIterable instanceof java.util.SortedSet<?>
                                           ? ((java.util.SortedSet<A>) javaIterable).comparator()
                                           : null;

        return treeSet(comparator == null ? naturalOrder() : comparator, javaIterable);
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
                        HashMultiSet.hashMultiSet(),
                        javaMap.entrySet());
    }

    /**
     * Construct a {@link HashMultiSet} from an input {@link Iterable}, using
     * {@link Objects#equals(Object, Object) Object equality} and {@link Objects#hashCode(Object) Object hashCode} as
     * the {@link EquivalenceRelation} and {@link HashingAlgorithm}, respectively, for its elements.
     *
     * @param javaIterable the input {@link Iterable}
     * @param <A>          the element type
     * @return the populated {@link HashMultiSet}
     */
    public static <A> HashMultiSet<A> hashMultiSet(Iterable<A> javaIterable) {
        return foldLeft(HashMultiSet::inc, HashMultiSet.hashMultiSet(), javaIterable);
    }

    /**
     * Construct a {@link TreeMultiSet} from an input {@link java.util.Map}, using the given {@link Comparator} for its
     * comparison relation.
     * <p>
     * Note that the resulting {@link TreeMultiSet} will only {@link TreeMultiSet#contains(Object) contain} elements
     * from the input {@link java.util.Map map} that mapped to a positive, non-zero integral value.
     *
     * @param comparator the {@link Comparator}
     * @param javaMap    the input {@link java.util.Map}
     * @param <A>        the element type
     * @return the populated {@link TreeMultiSet}
     */
    public static <A> TreeMultiSet<A> treeMultiSet(Comparator<? super A> comparator,
                                                   java.util.Map<A, Integer> javaMap) {
        return foldLeft(curried(tms -> into((a, k) -> atLeastZero(k).match(constantly(tms),
                                                                           nonZeroK -> tms.inc(a, nonZeroK)))),
                        TreeMultiSet.treeMultiSet(comparator),
                        javaMap.entrySet());
    }

    /**
     * Construct a {@link TreeMultiSet} from an input {@link java.util.Map}. If the input map is an instance of a
     * {@link SortedMap} with a non-null {@link Comparator}, that {@link Comparator} will be used for the key
     * comparison relation; otherwise, {@link Comparator#naturalOrder() natural ordering} will be used.
     * <p>
     * Note that the resulting {@link TreeMultiSet} will only {@link TreeMultiSet#contains(Object) contain} elements
     * from the input {@link java.util.Map map} that mapped to a positive, non-zero integral value.
     *
     * @param javaMap the input {@link java.util.Map}
     * @param <A>     the element type
     * @return the populated {@link TreeMultiSet}
     */
    public static <A extends Comparable<? super A>> TreeMultiSet<A> treeMultiSet(java.util.Map<A, Integer> javaMap) {
        Comparator<? super A> comparator = javaMap instanceof SortedMap<?, ?>
                                           ? ((SortedMap<A, Integer>) javaMap).comparator()
                                           : null;
        return treeMultiSet(comparator == null ? naturalOrder() : comparator, javaMap);
    }

    /**
     * Construct a {@link TreeMultiSet} from an input {@link Iterable}, using the given {@link Comparator} for its
     * comparison relation.
     * <p>
     * Note that the resulting {@link TreeMultiSet} will only {@link TreeMultiSet#contains(Object) contain} elements
     * from the input {@link java.util.Map map} that mapped to a positive, non-zero integral value.
     *
     * @param comparator   the {@link Comparator}
     * @param javaIterable the input {@link java.util.Map}
     * @param <A>          the element type
     * @return the populated {@link TreeMultiSet}
     */
    public static <A> TreeMultiSet<A> treeMultiSet(Comparator<? super A> comparator,
                                                   Iterable<A> javaIterable) {
        return foldLeft(TreeMultiSet::inc, TreeMultiSet.treeMultiSet(comparator), javaIterable);
    }

    /**
     * Construct a {@link TreeMultiSet} from an input {@link Iterable}. If the input iterable is an instance of a
     * {@link SortedSet} with a non-null {@link Comparator}, that {@link Comparator} will be used for the comparison
     * relation; otherwise, {@link Comparator#naturalOrder() natural ordering} will be used.
     * <p>
     * Note that the resulting {@link TreeMultiSet} will only {@link TreeMultiSet#contains(Object) contain} elements
     * from the input {@link java.util.Map map} that mapped to a positive, non-zero integral value.
     *
     * @param javaIterable the input {@link Iterable}
     * @param <A>          the {@link Comparable} element type
     * @return the populated {@link TreeMultiSet}
     */
    public static <A extends Comparable<? super A>> TreeMultiSet<A> treeMultiSet(Iterable<A> javaIterable) {
        Comparator<? super A> comparator = javaIterable instanceof SortedSet<?>
                                           ? ((SortedSet<A>) javaIterable).comparator()
                                           : null;
        return treeMultiSet(comparator == null ? naturalOrder() : comparator, javaIterable);
    }
}
