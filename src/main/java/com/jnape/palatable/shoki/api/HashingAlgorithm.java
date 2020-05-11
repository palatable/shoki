package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.lambda.functor.Applicative;

import java.util.Objects;

/**
 * A {@link HashingAlgorithm hashing algorithm} is an {@link Fn1 arrow} <code>A -&gt; Integer</code>. Good
 * hashing algorithms are generally fast and uniformly distribute values of type <code>A</code> across values of type
 * {@link Integer} (modulo cardinality differences where <code>A</code>'s cardinality exceeds {@link Integer}'s).
 *
 * @param <A> the type to hash
 */
public interface HashingAlgorithm<A> extends Fn1<A, Integer> {

    /**
     * {@inheritDoc}
     */
    @Override
    default HashingAlgorithm<A> local(Fn1<? super A, ? extends A> fn) {
        return Fn1.super.local(fn)::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default HashingAlgorithm<A> censor(Fn1<? super A, ? extends A> fn) {
        return Fn1.super.censor(fn)::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default <C> HashingAlgorithm<A> discardR(Applicative<C, Fn1<A, ?>> appB) {
        return Fn1.super.discardR(appB)::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default <Z> HashingAlgorithm<Z> diMapL(Fn1<? super Z, ? extends A> fn) {
        return Fn1.super.diMapL(fn)::apply;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default <Z> HashingAlgorithm<Z> contraMap(Fn1<? super Z, ? extends A> fn) {
        return Fn1.super.contraMap(fn)::apply;
    }

    /**
     * A {@link HashingAlgorithm} implemented in terms of {@link Objects#hashCode(Object)}.
     *
     * @param <A> the type to hash
     * @return a {@link HashingAlgorithm} implemented in terms of {@link Objects#hashCode(Object)}.
     */
    static <A> HashingAlgorithm<A> objectHashCode() {
        return Objects::hashCode;
    }

    /**
     * A {@link HashingAlgorithm} implemented in terms of {@link System#identityHashCode(Object)}.
     *
     * @param <A> the type to hash
     * @return a {@link HashingAlgorithm} implemented in terms of {@link System#identityHashCode(Object)}.
     */
    static <A> HashingAlgorithm<A> identityHashCode() {
        return System::identityHashCode;
    }


    /**
     * A {@link HashingAlgorithm} implemented in terms of {@link java.util.Arrays#hashCode(Object[])}.
     *
     * @param <A> the array component type
     * @return a {@link HashingAlgorithm} implemented in terms of {@link java.util.Arrays#hashCode(Object[])}.
     */
    static <A> HashingAlgorithm<A[]> arraysHashCode() {
        return java.util.Arrays::hashCode;
    }

    /**
     * A {@link HashingAlgorithm} implemented in terms of {@link java.util.Arrays#deepHashCode(Object[])}.
     *
     * @return a {@link HashingAlgorithm} implemented in terms of {@link java.util.Arrays#deepHashCode(Object[])}.
     */
    static HashingAlgorithm<Object[]> arraysDeepHashCode() {
        return java.util.Arrays::deepHashCode;
    }

    /**
     * Compute the hash of <code>a</code> in terms of the given {@link HashingAlgorithm hashingAlgorithm}.
     *
     * @param <A>              the value type
     * @param hashingAlgorithm the {@link HashingAlgorithm}
     * @param a                the value
     * @return the hash
     */
    static <A> int hash(HashingAlgorithm<? super A> hashingAlgorithm, A a) {
        return hashingAlgorithm.apply(a);
    }
}
