package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.SizeInfo.Known;
import com.jnape.palatable.shoki.api.OrderedCollection;
import com.jnape.palatable.shoki.api.Queue;
import com.jnape.palatable.shoki.api.Stack;

import java.util.Iterator;

import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static java.util.Arrays.asList;

/**
 * An immutable, structure-sharing implementation of {@link Queue} that can also be used as a {@link Stack}.
 *
 * @param <A> the element type
 */
public abstract class ImmutableQueue<A> implements Queue<Integer, A>, Stack<Integer, A> {

    private ImmutableQueue() {
    }

    /**
     * Produce a new {@link ImmutableQueue} instance with <code>a</code> added to the back. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link ImmutableQueue}
     */
    @Override
    public abstract ImmutableQueue<A> snoc(A a);

    /**
     * The remaining elements after removing the head of this {@link ImmutableQueue}, or {@link ImmutableQueue#empty()}
     * if there are no elements. Amortized <code>O(1)</code>.
     *
     * @return the tail of this {@link ImmutableQueue}
     */
    @Override
    public abstract ImmutableQueue<A> tail();

    /**
     * Reverse this {@link ImmutableQueue}. <code>O(1)</code>.
     *
     * @return this {@link ImmutableQueue}, reversed
     */
    @Override
    public abstract ImmutableQueue<A> reverse();

    /**
     * Produce a new {@link ImmutableQueue} instance with <code>a</code> added to the front. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link ImmutableQueue}
     */
    @Override
    public abstract ImmutableQueue<A> cons(A a);

    /**
     * Returns true if <code>other</code> is an {@link ImmutableQueue} with exactly the same elements in the same order
     * as this {@link ImmutableQueue} (although not necessarily in the same internally represented structure, regarding
     * incoming vs. outgoing); otherwise, returns false. <code>O(n)</code>.
     *
     * @param other the reference object with which to compare
     * @return true if the compared to a value-equal {@link ImmutableQueue}
     */
    @Override
    public boolean equals(Object other) {
        return other instanceof ImmutableQueue && OrderedCollection.equals(this, (ImmutableQueue<?>) other);
    }

    /**
     * Provide a debug-friendly string representation of this {@link ImmutableQueue}. <code>O(n)</code>
     *
     * @return the string representation of this {@link ImmutableQueue}
     */
    @Override
    public final String toString() {
        StringBuilder toString = new StringBuilder("ImmutableQueue[");

        Iterator<A> it = iterator();
        while (it.hasNext()) {
            toString.append(it.next());
            if (it.hasNext())
                toString.append(", ");
        }

        return toString.append("]").toString();
    }

    /**
     * The empty singleton instance of this {@link ImmutableQueue}.
     *
     * @param <A> the {@link ImmutableQueue} element type
     * @return an empty queue
     */
    @SuppressWarnings("unchecked")
    public static <A> ImmutableQueue<A> empty() {
        return (ImmutableQueue<A>) Empty.INSTANCE;
    }

    /**
     * Convenience static factory method to construct an {@link ImmutableQueue} from an {@link Iterable} of elements.
     * <code>O(n)</code>.
     *
     * @param as  the elements from front to back
     * @param <A> the {@link Iterable} and {@link ImmutableQueue} element type
     * @return the new {@link ImmutableQueue}
     */
    public static <A> ImmutableQueue<A> of(Iterable<A> as) {
        return foldLeft(ImmutableQueue::snoc, ImmutableQueue.empty(), as);
    }

    /**
     * Convenience static factory method to construct an {@link ImmutableQueue} from varargs elements.
     * <code>O(n)</code>.
     *
     * @param as  the elements from front to back
     * @param <A> the {@link ImmutableQueue} element type
     * @return the new {@link ImmutableQueue}
     */
    @SafeVarargs
    public static <A> ImmutableQueue<A> of(A... as) {
        return of(asList(as));
    }

    private static final class Empty<A> extends ImmutableQueue<A> {
        private static final Empty<?> INSTANCE = new Empty<>();

        private Empty() {
        }

        @Override
        public ImmutableQueue<A> reverse() {
            return this;
        }

        @Override
        public ImmutableQueue<A> snoc(A a) {
            return cons(a);
        }

        @Override
        public ImmutableQueue<A> tail() {
            return this;
        }

        @Override
        public ImmutableQueue<A> cons(A a) {
            return new NonEmpty<>(ImmutableStack.<A>empty().cons(a), ImmutableStack.empty());
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

    private static final class NonEmpty<A> extends ImmutableQueue<A> {
        private final ImmutableStack<A> outgoing;
        private final ImmutableStack<A> incoming;
        private final Known<Integer>    sizeInfo;
        private final int               hashCode;

        private NonEmpty(ImmutableStack<A> outgoing, ImmutableStack<A> incoming) {
            this.outgoing = outgoing;
            this.incoming = incoming;
            sizeInfo = known(outgoing.sizeInfo().getSize() + incoming.sizeInfo().getSize());
            hashCode = 31 * outgoing.hashCode() + incoming.hashCode();
        }

        @Override
        public ImmutableQueue<A> reverse() {
            return incoming.isEmpty()
                ? new NonEmpty<>(outgoing.reverse(), incoming)
                : new NonEmpty<>(incoming, outgoing);
        }

        @Override
        public ImmutableQueue<A> cons(A a) {
            return new NonEmpty<>(outgoing.cons(a), incoming);
        }

        @Override
        public ImmutableQueue<A> tail() {
            ImmutableStack<A> outgoingTail = outgoing.tail();
            if (!outgoingTail.isEmpty())
                return new NonEmpty<>(outgoingTail, incoming);

            return incoming.isEmpty()
                   ? ImmutableQueue.empty()
                   : new NonEmpty<>(foldLeft(ImmutableStack::cons, ImmutableStack.empty(), incoming),
                                    ImmutableStack.empty());
        }

        @Override
        public ImmutableQueue<A> snoc(A a) {
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
