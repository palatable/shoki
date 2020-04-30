package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.functions.builtin.fn2.Cons;
import com.jnape.palatable.shoki.api.Collection;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.SizeInfo;
import com.jnape.palatable.shoki.api.SizeInfo.Known;
import com.jnape.palatable.shoki.api.Stack;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Size.size;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.sameElementsSameOrder;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static java.util.Arrays.asList;

/**
 * A strictly-evaluated, structure-sharing implementation of {@link Stack}.
 *
 * @param <A> the element type
 * @see StrictQueue
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
    public final StrictStack<A> cons(A a) {
        return new Head<>(a, this);
    }

    /**
     * The remaining elements after removing the head of this {@link StrictStack}, or {@link StrictStack#empty()}
     * if there are no elements. <code>O(1)</code>.
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
        return foldLeft(StrictStack::cons, StrictStack.empty(), this);
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
     * The {@link SizeInfo} of this {@link StrictStack}. Amortized <code>O(1)</code>.
     */
    @Override
    public abstract Known<Natural> sizeInfo();

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
                equivalent(this, downcast(other), sameElementsSameOrder(objectEquals()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final Iterator<A> iterator() {
        return new Iterator<A>() {
            StrictStack<A> rest = StrictStack.this;

            @Override
            public boolean hasNext() {
                return rest instanceof Head<?>;
            }

            @Override
            public A next() {
                if (!hasNext())
                    throw new NoSuchElementException();

                Head<A> head = (Head<A>) this.rest;
                A       next = head.head;
                rest = head.tail;
                return next;
            }
        };
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
     * Provide a debug-friendly string representation of this {@link StrictStack}. <code>O(n)</code>
     *
     * @return the string representation of this {@link StrictStack}
     */
    @Override
    public final String toString() {
        StringBuilder body = new StringBuilder("StrictStack[");

        StrictStack<A> next = this;
        while (next != Empty.INSTANCE) {
            Head<A> head = (Head<A>) next;
            body.append(head.head);
            next = head.tail;
            if (next != Empty.INSTANCE)
                body.append(", ");
        }

        return body.append("]").toString();
    }

    /**
     * The empty singleton instance of this {@link StrictStack}. <code>O(1)</code>.
     *
     * @param <A> the {@link StrictStack} element type
     * @return an empty stack
     */
    @SuppressWarnings("unchecked")
    public static <A> StrictStack<A> empty() {
        return (StrictStack<A>) Empty.INSTANCE;
    }

    /**
     * Convenience static factory method to construct an {@link StrictStack} from varargs elements. <code>O(n)</code>.
     *
     * @param a   the first element to {@link StrictStack#cons(Object) cons}
     * @param as  the remaining elements to {@link StrictStack#cons(Object) cons} from back to front
     * @param <A> the {@link StrictStack} element type
     * @return the new {@link StrictStack}
     */
    @SafeVarargs
    public static <A> StrictStack<A> of(A a, A... as) {
        return foldLeft(StrictStack::cons, empty(), Cons.cons(a, asList(as)));
    }

    private static final class Head<A> extends StrictStack<A> {
        private final A              head;
        private final StrictStack<A> tail;

        private volatile Natural size;
        private volatile Integer hashCode;

        private Head(A head, StrictStack<A> tail) {
            this.head = head;
            this.tail = tail;
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
            return tail;
        }

        @Override
        @SuppressWarnings("DuplicatedCode")
        public Known<Natural> sizeInfo() {
            Natural size = this.size;
            if (size == null) {
                synchronized (this) {
                    size = this.size;
                    if (size == null) {
                        this.size = size = abs(size(this));
                    }
                }
            }
            return known(size);
        }

        @Override
        public int hashCode() {
            Integer hashCode = this.hashCode;
            if (hashCode == null) {
                synchronized (this) {
                    hashCode = this.hashCode;
                    if (hashCode == null) {
                        this.hashCode = hashCode = foldLeft((hc, a) -> hc * 31 + Objects.hashCode(a), 0, this);
                    }
                }
            }
            return hashCode;
        }
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
        public Known<Natural> sizeInfo() {
            return known(zero());
        }

        @Override
        public int hashCode() {
            return 0;
        }
    }
}
