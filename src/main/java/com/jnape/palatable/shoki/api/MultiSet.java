package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.semigroup.Semigroup;
import com.jnape.palatable.shoki.api.Natural.NonZero;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.Fn2.curried;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn2.GTE.gte;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.monoid.builtin.And.and;
import static com.jnape.palatable.lambda.semigroup.builtin.Max.max;
import static com.jnape.palatable.lambda.semigroup.builtin.Min.min;
import static com.jnape.palatable.shoki.api.Natural.one;

/**
 * A {@link MultiSet} (also referred to as a <em>Bag</em>) is a {@link Collection} of {@link Tuple2 pairs} of contained
 * elements and their {@link NonZero non-zero} multiplicity in the {@link MultiSet}, supporting {@link RandomAccess}
 * from a possible element to its {@link Natural} multiplicity.
 *
 * @param <A> the element type
 */
public interface MultiSet<A> extends Collection<Natural, Tuple2<A, NonZero>>, RandomAccess<A, Natural> {

    /**
     * Add <code>k</code> occurrences of <code>a</code> to this {@link MultiSet}.
     *
     * @param a the element to add
     * @param k the number of occurrences of the element to add
     * @return the updated {@link MultiSet}
     */
    MultiSet<A> add(A a, NonZero k);

    /**
     * Remove <code><em>min</em>(k, {@link MultiSet#get get}(a))</code> occurrences of <code>a</code> from this
     * {@link MultiSet}.
     *
     * @param a the element to remove
     * @param k the number of occurrences of the element to remove
     * @return the updated {@link MultiSet}
     */
    MultiSet<A> remove(A a, NonZero k);

    /**
     * The ({@link Natural possibly zero}) multiplicity (number of occurrences) of <code>a</code> in this
     * {@link MultiSet}.
     *
     * @param a the element for which to lookup the multiplicity
     * @return the multiplicity of a in this {@link MultiSet}
     */
    @Override
    Natural get(A a);

    /**
     * {@inheritDoc}
     *
     * @return this {@link MultiSet} without the {@link MultiSet#head() head} occurrence pair
     */
    @Override
    MultiSet<A> tail();

    /**
     * {@link MultiSet#add(Object, NonZero) Add} {@link Natural#one() one} occurrence of <code>a</code> to this
     * {@link MultiSet}.
     *
     * @param a the element to add
     * @return the updated {@link MultiSet}
     * @see MultiSet#add(Object, NonZero)
     */
    default MultiSet<A> add(A a) {
        return add(a, one());
    }

    /**
     * {@link MultiSet#remove(Object, NonZero) Remove}
     * <code><em>min</em>({@link Natural#one() one}, {@link MultiSet#get get}(a))</code> occurrences of <code>a</code>
     * from this {@link MultiSet}.
     *
     * @param a the element to remove
     * @return the updated {@link MultiSet}
     * @see MultiSet#remove(Object, NonZero)
     */
    default MultiSet<A> remove(A a) {
        return remove(a, one());
    }

    /**
     * {@link MultiSet#remove(Object, NonZero) Remove} all occurrences of <code>a</code> from this {@link MultiSet},
     * such that subsequent invocations of <code>{@link MultiSet#get(Object) get}(a)</code> return
     * {@link Natural#zero() zero}.
     * <p>
     * By default, this method simply {@link MultiSet#remove(Object, NonZero) removes} the
     * {@link MultiSet#get(Object) multiplicity} of <code>a</code> from the {@link MultiSet}, although specific
     * implementations may be able to perform this operation more directly and efficiently.
     *
     * @param a the element to remove
     * @return the updated {@link MultiSet}
     * @see MultiSet#remove(Object, NonZero)
     */
    default MultiSet<A> removeAll(A a) {
        return get(a).match(constantly(this), k -> remove(a, k));
    }

    /**
     * {@link MultiSet#add Add} the {@link MultiSet#get(Object) multiplicity} of each element in <code>other</code>
     * to that element's current {@link MultiSet#get(Object) multiplicity} in this {@link MultiSet}.
     *
     * @param other the {@link MultiSet} from which to {@link MultiSet#add(Object, NonZero) add} elements
     * @return the updated {@link MultiSet}
     */
    default MultiSet<A> addAll(MultiSet<A> other) {
        return foldLeft(curried(ms -> into(ms::add)), this, other);
    }


    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     */
    @Override
    default boolean contains(A a) {
        return contains(a, one());
    }

    /**
     * Determine if some value <code>k</code> <code>a</code>s are elements of this {@link MultiSet}.
     * Equivalent to <code>get(a) >= k</code>
     *
     * @param a the value
     * @param k the amount of a
     * @return true if there are k <code>a</code>s in this {@link MultiSet}; false otherwise
     */
    default boolean contains(A a, NonZero k) {
        return gte(k, get(a));
    }

    /**
     * The union of two {@link MultiSet MultiSets}
     * <code>xs</code> and <code>ys</code> is the {@link MultiSet} of those elements present in either <code>xs</code>
     * or <code>ys</code> with the occurrences of whichever one had more elements. This is equivalent to
     * {@link MultiSet#merge} partially applied with {@link com.jnape.palatable.lambda.semigroup.builtin.Max#max}.
     *
     * @param other the {@link MultiSet} to union with this {@link MultiSet}
     * @return the union {@link MultiSet}
     * @see MultiSet#merge
     */
    default MultiSet<A> union(MultiSet<A> other) {
        return merge(max(), other);
    }

    /**
     * The <a href="https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement"
     * target="_new">difference</a> (also known as "relative complement") of two {@link MultiSet MultiSets}
     * <code>xs</code> and <code>ys</code> is the {@link MultiSet} of in <code>xs</code> with all elements of
     * <code>ys</code> removed.
     *
     * @param other the {@link MultiSet} to subtract from this {@link MultiSet}
     * @return the difference {@link MultiSet}
     */
    default MultiSet<A> difference(MultiSet<A> other) {
        return foldLeft(curried(ms -> into(ms::remove)), this, other);
    }

    /**
     * Determine if another {@link MultiSet} is a subset of this {@link MultiSet}. A {@link MultiSet} <code>x</code> is
     * a subset of <code>y</code> if for every <code>a^i</code> in <code>x</code> there exists <code>a^j</code> in
     * <code>y</code> where ^ denotes the number of occurrences in the multiset.
     *
     * @param other the {@link MultiSet} to test whether it's a subset
     * @return whether other is a subset
     */
    default boolean inclusion(MultiSet<A> other) {
        return and().foldMap(into(this::contains), other);
    }

    /**
     * The intersection of two {@link MultiSet MultiSets}
     * <code>xs</code> and <code>ys</code> is the {@link MultiSet} of those elements present in both <code>xs</code>
     * and <code>ys</code> with the occurrences of whichever one had less elements. This is equivalent to
     * {@link MultiSet#merge} partially applied with {@link com.jnape.palatable.lambda.semigroup.builtin.Min#min}.
     *
     * @param other the {@link MultiSet} to intersect with this {@link MultiSet}
     * @return the intersected {@link MultiSet}
     * @see MultiSet#merge
     */
    default MultiSet<A> intersection(MultiSet<A> other) {
        return merge(min(), other);
    }


    /**
     * The symmetric difference of two {@link MultiSet MultiSets} <code>xs</code> and <code>ys</code> is the
     * {@link MultiSet} of the occurrence elements present in either <code>xs</code> or <code>ys</code> but not the
     * other.
     *
     * @param other the {@link MultiSet} to symmetrically subtract from this {@link MultiSet}
     * @return the {@link MultiSet} containing the symmetric difference
     * @see MultiSet#merge
     */
    default MultiSet<A> symmetricDifference(MultiSet<A> other) {
        return difference(other).union(other.difference(this));
    }

    /**
     * {@link Set} containing the unique elements from the {@link MultiSet}
     *
     * @return the unique {@link Set}
     */
    Set<Natural, A> unique();

    /**
     * Combine {@link MultiSet}s with a {@link Semigroup} to handle the combining of the amounts.
     * Note that if one of the {@link MultiSet}s does not contain an {@link A} it will still pass
     * {@link com.jnape.palatable.shoki.api.Natural.Zero} to the {@link Semigroup}.
     *
     * @param semigroup the {@link Semigroup}
     * @param other     the other {@link MultiSet}
     * @return the merged {@link MultiSet}
     */
    default MultiSet<A> merge(Semigroup<Natural> semigroup, MultiSet<A> other) {
        return foldLeft((acc, a) -> {
                            Natural ourAs = get(a);
                            return semigroup
                                    .apply(ourAs, other.get(a))
                                    .match(__ -> acc.removeAll(a),
                                           nz -> nz.minus(ourAs)
                                                   .flatMap(n -> n.match(constantly(nothing()),
                                                                         diff -> just(acc.add(a, diff))))
                                                   .fmap(Maybe::just)
                                                   .orElseGet(() -> ourAs.minus(nz)
                                                           .flatMap(n -> n.match(constantly(nothing()),
                                                                                 diff -> just(acc.remove(a, diff)))))
                                                   .orElse(acc));
                        },
                        this,
                        other.unique().union(this.unique()));
    }

    /**
     * Common {@link EquivalenceRelation}s between {@link MultiSet MultiSets}.
     */
    final class EquivalenceRelations {
        private EquivalenceRelations() {
        }

        /**
         * An {@link EquivalenceRelation} between two {@link MultiSet}s that holds if, and only if, both
         * {@link MultiSet MultiSets} have the same elements with the same multiplicity. <code>O(n)</code>.
         *
         * @param <A> the element type
         * @param <S> the {@link MultiSet} subtype of the arguments
         * @return the {@link EquivalenceRelation}
         */
        public static <A, S extends MultiSet<A>> EquivalenceRelation<S> sameElementsSameMultiplicity() {
            EquivalenceRelation<S> sameMembershipMultiplicity = (xs, ys) -> and().foldMap(into(ys::contains), xs);
            return Sizable.EquivalenceRelations.<S>sameSizes().and(sameMembershipMultiplicity);
        }
    }
}
