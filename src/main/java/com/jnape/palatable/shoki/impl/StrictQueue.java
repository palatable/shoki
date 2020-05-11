package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.Collection;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Queue;
import com.jnape.palatable.shoki.api.SizeInfo.Known;
import com.jnape.palatable.shoki.api.Stack;

import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.arraysHashCode;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.hash;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.elementsInOrder;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static java.util.Collections.emptyIterator;

/**
 * A strictly-evaluated {@link Queue} that can also be used as a {@link Stack}.
 *
 * @param <A> the element type
 * @see StrictStack
 */
public abstract class StrictQueue<A> implements Queue<Natural, A>, Stack<Natural, A> {

    private StrictQueue() {
    }

    /**
     * {@inheritDoc}
     * <code>O(k)</code>.
     */
    @Override
    public StrictQueue<A> consAll(Collection<Natural, A> other) {
        return (StrictQueue<A>) Stack.super.consAll(other);
    }

    /**
     * {@inheritDoc}
     * <code>O(k)</code>.
     */
    @Override
    public StrictQueue<A> snocAll(Collection<Natural, A> collection) {
        return (StrictQueue<A>) Queue.super.snocAll(collection);
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
     * The remaining elements after removing the head of this {@link StrictQueue}, or an empty {@link StrictQueue} if
     * there are no elements. Amortized <code>O(1)</code>.
     *
     * @return the tail of this {@link StrictQueue}
     */
    @Override
    public abstract StrictQueue<A> tail();

    /**
     * Reverse this {@link StrictQueue}. Amortized <code>O(1)</code>.
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
        return other instanceof StrictQueue<?> &&
                equivalent(elementsInOrder(objectEquals()), this, downcast(other));
    }

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     *
     * @return the hash code
     */
    @Override
    public abstract int hashCode();

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
     * Create a {@link StrictQueue} of zero or more elements, with the elements queued for removal from left to right.
     * <code>O(n)</code>.
     *
     * @param as  the elements to {@link StrictQueue#snoc(Object) snoc} from front to back
     * @param <A> the element type
     * @return the {@link StrictQueue}
     */
    @SafeVarargs
    public static <A> StrictQueue<A> strictQueue(A... as) {
        @SuppressWarnings("unchecked")
        StrictQueue<A> empty = (StrictQueue<A>) Empty.INSTANCE;
        return as.length == 0
               ? empty
               : new NonEmpty<>(strictStack(as), strictStack());
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
            return new NonEmpty<>(strictStack(a), strictStack());
        }

        @Override
        public Maybe<A> head() {
            return nothing();
        }

        @Override
        public Known<Natural> sizeInfo() {
            return known(zero());
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int hashCode() {
            return 0;
        }

        @Override
        public Iterator<A> iterator() {
            return emptyIterator();
        }
    }

    private static final class NonEmpty<A> extends StrictQueue<A> {
        private final StrictStack<A> outgoing;
        private final StrictStack<A> incoming;

        private volatile Natural size;
        private volatile Integer hashCode;

        private NonEmpty(StrictStack<A> outgoing, StrictStack<A> incoming) {
            this.outgoing = outgoing;
            this.incoming = incoming;
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
                   ? strictQueue()
                   : new NonEmpty<>(incoming.reverse(), strictStack());
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
        public Known<Natural> sizeInfo() {
            Natural size = this.size;
            if (size == null) {
                synchronized (this) {
                    size = this.size;
                    if (size == null) {
                        this.size = size = outgoing.sizeInfo().getSize().plus(incoming.sizeInfo().getSize());
                    }
                }
            }
            return known(size);
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public int hashCode() {
            Integer hashCode = this.hashCode;
            if (hashCode == null) {
                synchronized (this) {
                    hashCode = this.hashCode;
                    if (hashCode == null) {
                        this.hashCode = hashCode = hash(arraysHashCode(), new StrictStack[]{outgoing, incoming});
                    }
                }
            }
            return hashCode;
        }

        @Override
        public Iterator<A> iterator() {
            return new Iterator<A>() {
                final Iterator<A> outgoing = NonEmpty.this.outgoing.iterator();
                Iterator<A> incoming;

                @Override
                public boolean hasNext() {
                    if (outgoing.hasNext())
                        return true;
                    if (incoming == null)
                        incoming = NonEmpty.this.incoming.reverse().iterator();
                    return incoming.hasNext();
                }

                @Override
                public A next() {
                    if (!hasNext())
                        throw new NoSuchElementException();
                    return outgoing.hasNext() ? outgoing.next() : incoming.next();
                }
            };
        }
    }
}
