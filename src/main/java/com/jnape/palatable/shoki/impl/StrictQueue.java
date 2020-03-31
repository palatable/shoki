package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.functions.builtin.fn2.Cons;
import com.jnape.palatable.shoki.api.OrderedCollection;
import com.jnape.palatable.shoki.api.Queue;
import com.jnape.palatable.shoki.api.SizeInfo.Known;
import com.jnape.palatable.shoki.api.Stack;

import java.util.Iterator;

import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static java.util.Arrays.asList;

/**
 * A strictly-evaluated, structure-sharing implementation of a {@link Queue} that can also be used as a {@link Stack}.
 *
 * @param <A> the element type
 * @see StrictStack
 */
public abstract class StrictQueue<A> implements Queue<Integer, A>, Stack<Integer, A> {

    private StrictQueue() {
    }

    /**
     * Produce a new {@link StrictQueue} instance with <code>a</code> added to the back. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link StrictQueue}
     */
    @Override
    public abstract StrictQueue<A> snoc(A a);

    /**
     * The remaining elements after removing the head of this {@link StrictQueue}, or {@link StrictQueue#empty()}
     * if there are no elements. Amortized <code>O(1)</code>.
     *
     * @return the tail of this {@link StrictQueue}
     */
    @Override
    public abstract StrictQueue<A> tail();

    /**
     * Reverse this {@link StrictQueue}. <code>O(1)</code>.
     *
     * @return this {@link StrictQueue}, reversed
     */
    @Override
    public abstract StrictQueue<A> reverse();

    /**
     * Produce a new {@link StrictQueue} instance with <code>a</code> added to the front. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link StrictQueue}
     */
    @Override
    public abstract StrictQueue<A> cons(A a);

    /**
     * Returns true if <code>other</code> is an {@link StrictQueue} with exactly the same elements in the same order
     * as this {@link StrictQueue} (although not necessarily in the same internally represented structure, regarding
     * incoming vs. outgoing); otherwise, returns false. <code>O(n)</code>.
     *
     * @param other the reference object with which to compare
     * @return true if the compared to a value-equal {@link StrictQueue}
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof StrictQueue && OrderedCollection.equals(this, (StrictQueue<?>) other);
    }

    /**
     * Provide a debug-friendly string representation of this {@link StrictQueue}. <code>O(n)</code>
     *
     * @return the string representation of this {@link StrictQueue}
     */
    @Override
    public final String toString() {
        StringBuilder toString = new StringBuilder("StrictQueue[");

        Iterator<A> it = iterator();
        while (it.hasNext()) {
            toString.append(it.next());
            if (it.hasNext())
                toString.append(", ");
        }

        return toString.append("]").toString();
    }

    /**
     * The empty singleton instance of this {@link StrictQueue}.
     *
     * @param <A> the {@link StrictQueue} element type
     * @return an empty queue
     */
    @SuppressWarnings("unchecked")
    public static <A> StrictQueue<A> empty() {
        return (StrictQueue<A>) Empty.INSTANCE;
    }

    /**
     * Convenience static factory method to construct an {@link StrictQueue} from varargs elements.
     * <code>O(n)</code>.
     *
     * @param as  the elements from front to back
     * @param <A> the {@link StrictQueue} element type
     * @return the new {@link StrictQueue}
     */
    @SafeVarargs
    public static <A> StrictQueue<A> of(A a, A... as) {
        return foldLeft(StrictQueue::snoc, StrictQueue.empty(), Cons.cons(a, asList(as)));
    }

    private static final class Empty<A> extends StrictQueue<A> {
        private static final Empty<?> INSTANCE = new Empty<>();

        private Empty() {
        }

        @Override
        public StrictQueue<A> reverse() {
            return this;
        }

        @Override
        public StrictQueue<A> snoc(A a) {
            return cons(a);
        }

        @Override
        public StrictQueue<A> tail() {
            return this;
        }

        @Override
        public StrictQueue<A> cons(A a) {
            return new NonEmpty<>(StrictStack.<A>empty().cons(a), StrictStack.empty());
        }

        @Override
        public Maybe<A> head() {
            return nothing();
        }

        @Override
        public Known<Integer> sizeInfo() {
            return known(0);
        }

        @Override
        public boolean isEmpty() {
            return true;
        }
    }

    private static final class NonEmpty<A> extends StrictQueue<A> {
        private final StrictStack<A> outgoing;
        private final StrictStack<A> incoming;
        private final Known<Integer> sizeInfo;
        private final int            hashCode;

        private NonEmpty(StrictStack<A> outgoing, StrictStack<A> incoming) {
            this.outgoing = outgoing;
            this.incoming = incoming;
            sizeInfo = known(outgoing.sizeInfo().getSize() + incoming.sizeInfo().getSize());
            hashCode = 31 * outgoing.hashCode() + incoming.hashCode();
        }

        @Override
        public StrictQueue<A> reverse() {
            return incoming.isEmpty()
                   ? new NonEmpty<>(outgoing.reverse(), incoming)
                   : new NonEmpty<>(incoming, outgoing);
        }

        @Override
        public StrictQueue<A> cons(A a) {
            return new NonEmpty<>(outgoing.cons(a), incoming);
        }

        @Override
        public StrictQueue<A> tail() {
            StrictStack<A> outgoingTail = outgoing.tail();
            if (!outgoingTail.isEmpty())
                return new NonEmpty<>(outgoingTail, incoming);

            return incoming.isEmpty()
                   ? StrictQueue.empty()
                   : new NonEmpty<>(foldLeft(StrictStack::cons, StrictStack.empty(), incoming),
                                    StrictStack.empty());
        }

        @Override
        public StrictQueue<A> snoc(A a) {
            return new NonEmpty<>(outgoing, incoming.cons(a));
        }

        @Override
        public Maybe<A> head() {
            return outgoing.head();
        }

        @Override
        public Known<Integer> sizeInfo() {
            return sizeInfo;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int hashCode() {
            return hashCode;
        }
    }
}
