package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.api.Collection;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Natural.NonZero;
import com.jnape.palatable.shoki.api.SizeInfo;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;
import com.jnape.palatable.shoki.api.Stack;

import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.Natural.atLeastOne;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.elementsInOrder;
import static com.jnape.palatable.shoki.api.SizeInfo.finite;
import static com.jnape.palatable.shoki.api.Value.known;

/**
 * A strictly-evaluated {@link Stack}.
 *
 * @param <A> the element type
 * @see Stack
 * @see AmortizedStack
 */
public abstract class StrictStack<A> implements Stack<Natural, A> {

    private StrictStack() {
    }

    /**
     * Produce a new {@link StrictStack} instance with <code>a</code> added to the front. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link StrictStack}
     */
    @Override
    public abstract StrictStack<A> cons(A a);

    /**
     * The remaining elements after removing the head of this {@link StrictStack}, or an empty {@link StrictStack} if
     * there is no head. <code>O(1)</code>.
     *
     * @return the tail of this {@link StrictStack}
     */
    @Override
    public abstract StrictStack<A> tail();

    /**
     * Reverse this {@link StrictStack}. <code>O(n)</code>.
     *
     * @return this {@link StrictStack}, reversed
     */
    @Override
    public StrictStack<A> reverse() {
        return foldLeft(StrictStack::cons, strictStack(), this);
    }

    /**
     * If this {@link StrictStack} is not empty, return the head element wrapped in {@link Maybe}. Otherwise, return
     * {@link Maybe#nothing()}. <code>O(1)</code>.
     *
     * @return {@link Maybe} the head element of this {@link StrictStack}
     */
    @Override
    public abstract Maybe<A> head();

    /**
     * The {@link SizeInfo} of this {@link StrictStack}. <code>O(1)</code>.
     */
    @Override
    public abstract Finite<Natural> sizeInfo();

    /**
     * Returns true if this {@link StrictStack} is empty; otherwise, returns false. <code>O(1)</code>.
     *
     * @return whether or not this {@link StrictStack} is empty
     */
    @Override
    public abstract boolean isEmpty();

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public StrictStack<A> consAll(Collection<Natural, A> other) {
        return (StrictStack<A>) Stack.super.consAll(other);
    }

    /**
     * Returns true if <code>other</code> is an {@link StrictStack} with exactly the same elements in the same order
     * as this {@link StrictStack}; otherwise, returns false. <code>O(n)</code>.
     *
     * @param other the reference object with which to compare
     * @return true if the compared to a value-equal {@link StrictStack}
     */
    @Override
    public final boolean equals(Object other) {
        return other instanceof StrictStack<?> &&
                equivalent(elementsInOrder(objectEquals()), this, downcast(other));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public abstract Iterator<A> iterator();

    /**
     * {@inheritDoc}
     * Amortized <code>O(1)</code>.
     *
     * @return the hash code
     */
    @Override
    public abstract int hashCode();

    /**
     * Provide a debug-friendly string representation of this {@link StrictStack}. <code>O(n)</code>
     *
     * @return the string representation of this {@link StrictStack}
     */
    @Override
    public abstract String toString();

    /**
     * Create a {@link StrictStack} of zero or more elements, with the elements queued for removal from left to right.
     * <code>O(n)</code>.
     *
     * @param as  the elements to {@link StrictStack#cons(Object) cons} from back to front
     * @param <A> the element type
     * @return the {@link StrictStack}
     */
    @SafeVarargs
    public static <A> StrictStack<A> strictStack(A... as) {
        if (as.length == 0)
            return Empty.empty();

        int     hashCode = 0;
        Node<A> tail     = null;
        for (int i = as.length - 1; i > 0; i--) {
            A a = as[i];
            tail     = new Node<>(a, tail);
            hashCode = (hashCode * 31) + a.hashCode();
        }
        return new NonEmpty<>(as[0], tail, atLeastOne(as.length), hashCode);
    }

    private static final class Empty<A> extends StrictStack<A> {
        private static final Empty<?> INSTANCE = new Empty<>();

        private Empty() {
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public Maybe<A> head() {
            return nothing();
        }

        @Override
        public StrictStack<A> tail() {
            return this;
        }

        @Override
        public StrictStack<A> cons(A a) {
            return new NonEmpty<>(a, null, one(), a.hashCode());
        }

        @Override
        public Finite<Natural> sizeInfo() {
            return finite(known(zero()));
        }

        @Override
        public int hashCode() {
            return 0;
        }

        @Override
        public Iterator<A> iterator() {
            return Collections.emptyIterator();
        }

        @Override
        public String toString() {
            return "StrictStack[]";
        }

        @SuppressWarnings("unchecked")
        private static <A> StrictStack<A> empty() {
            return (StrictStack<A>) Empty.INSTANCE;
        }
    }

    private static final class NonEmpty<A> extends StrictStack<A> {

        private final A       head;
        private final Node<A> tail;
        private final NonZero size;
        private final Integer hashCode;

        private NonEmpty(A head, Node<A> tail, NonZero size, Integer hashCode) {
            this.head     = head;
            this.tail     = tail;
            this.size     = size;
            this.hashCode = hashCode;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public Maybe<A> head() {
            return just(head);
        }

        @Override
        public StrictStack<A> tail() {
            return tail == null
                   ? Empty.empty()
                   : new NonEmpty<>(tail.a, tail.tail, size.decOrOne(), (hashCode - head.hashCode()) / 31);
        }

        @Override
        public StrictStack<A> cons(A newHead) {
            return new NonEmpty<>(newHead,
                                  new Node<>(head, tail),
                                  size.inc(),
                                  (hashCode * 31) + newHead.hashCode());
        }

        @Override
        public Finite<Natural> sizeInfo() {
            return finite(known(size));
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public Iterator<A> iterator() {
            return new Iterator<A>() {
                private Node<A> node = new Node<>(head, tail);

                @Override
                public boolean hasNext() {
                    return node != null;
                }

                @Override
                public A next() {
                    if (!hasNext())
                        throw new NoSuchElementException();

                    A next = node.a;
                    node = node.tail;
                    return next;
                }
            };
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("StrictStack[").append(head);

            Node<A> node = tail;
            while (node != null) {
                sb.append(", ").append(node.a);
                node = node.tail;
            }
            return sb.append(']').toString();
        }
    }

    private static final class Node<A> {

        private final A       a;
        private final Node<A> tail;

        private Node(A a, Node<A> tail) {
            this.a    = a;
            this.tail = tail;
        }
    }
}
