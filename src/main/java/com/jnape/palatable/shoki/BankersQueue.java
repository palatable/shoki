package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;

import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static com.jnape.palatable.shoki.Stream.*;
import static java.util.Arrays.asList;

/**
 * A Banker's Queue, or a persistent queue offering amortized O(1) for {@link BankersQueue#head() head} /
 * {@link BankersQueue#head() tail} / {@link BankersQueue#head() snoc}.
 *
 * @param <A> the element type
 */
public final class BankersQueue<A> implements Queue<Integer, A> {

    private final $<? extends Stream<A>> $f;
    private final int                    lenF;
    private final $<? extends Stream<A>> $r;
    private final int                    lenR;

    private BankersQueue($<? extends Stream<A>> $f, int lenF, $<? extends Stream<A>> $r, int lenR) {
        this.$f = $f;
        this.lenF = lenF;
        this.$r = $r;
        this.lenR = lenR;
    }

    /**
     * Return {@link Maybe maybe} the head of this {@link BankersQueue}. <code>O(1)</code>.
     *
     * @return {@link Maybe maybe} the head of this {@link BankersQueue}
     */
    @Override
    public Maybe<A> head() {
        return isEmpty() ? nothing() : $f.force().projectB().fmap(Stream.Cons::head);
    }

    /**
     * The remaining elements after removing the head of this {@link BankersQueue} and discharging all front-loaded
     * debited suspensions, or {@link BankersQueue#empty()} if there are no elements. Amortized <code>O(1)</code>.
     *
     * @return the tail of this {@link BankersQueue}
     */
    @Override
    public BankersQueue<A> tail() {
        return isEmpty()
            ? this
            : $f.force().projectB().match(constantly(this),
                                          cons -> queue(cons.tail(), lenF - 1, $r, lenR));
    }

    /**
     * Produce a new {@link BankersQueue} instance with <code>a</code> added to the back, front-loading one credited
     * computation. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link BankersQueue}
     */
    @Override
    public BankersQueue<A> snoc(A a) {
        return queue($f, lenF, $cons(a, $r), lenR + 1);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SizeInfo.Known<Integer> sizeInfo() {
        return known(lenF + lenR);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty() {
        return lenF == 0;
    }

    /**
     * Reverse this {@link BankersQueue}. Amortized <code>O(1)</code>.
     *
     * @return this {@link BankersQueue}, reversed
     */
    @Override
    public BankersQueue<A> reverse() {
        return queue($r, lenR, $f, lenF);
    }

    @Override
    public String toString() {
        return "BankersQueue{" +
            "$f=" + $f +
            ", lenF=" + lenF +
            ", $r=" + $r +
            ", lenR=" + lenR +
            '}';
    }

    public static <A> BankersQueue<A> empty() {
        return new BankersQueue<>($nil(), 0, $nil(), 0);
    }

    @SafeVarargs
    public static <A> BankersQueue<A> of(A a, A... as) {
        return foldLeft(BankersQueue::snoc, BankersQueue.<A>empty().snoc(a), asList(as));
    }

    private static <A> BankersQueue<A> queue($<? extends Stream<A>> $f, int lenF,
                                             $<? extends Stream<A>> $r, int lenR) {
        return lenR <= lenF
            ? new BankersQueue<>($f, lenF, $r, lenR)
            : new BankersQueue<>(concat($f, Stream.reverse($r)), lenF + lenR, $nil(), 0);
    }
}
