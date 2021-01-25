package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.functions.builtin.fn1.Downcast;
import com.jnape.palatable.shoki.api.Collection;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.SizeInfo;
import com.jnape.palatable.shoki.api.SizeInfo.Sized.Finite;
import com.jnape.palatable.shoki.api.Stack;
import com.jnape.palatable.shoki.api.Value;
import com.jnape.palatable.shoki.api.Value.Computed;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.hash;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Memo.volatileField;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.elementsInOrder;
import static com.jnape.palatable.shoki.api.OrderedCollection.HashingAlgorithms.elementsInOrder;
import static com.jnape.palatable.shoki.api.SizeInfo.finite;
import static com.jnape.palatable.shoki.api.Value.computedOnce;
import static java.util.concurrent.atomic.AtomicReferenceFieldUpdater.newUpdater;

/**
 * A {@link Stack} that amortizes the cost of size and hash code computations via {@link Computed.Once once-computed}
 * {@link Value values}.
 *
 * @param <A> the element type
 * @see Stack
 * @see StrictStack
 */
public abstract class AmortizedStack<A> implements Stack<Natural, A> {

    private AmortizedStack() {
    }

    /**
     * Produce a new {@link AmortizedStack} instance with <code>a</code> added to the front. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link AmortizedStack}
     */
    @Override
    public final AmortizedStack<A> cons(A a) {
        return new Head<>(a, this);
    }

    /**
     * The remaining elements after removing the head of this {@link AmortizedStack}, or an empty {@link AmortizedStack}
     * if there is no head. <code>O(1)</code>.
     *
     * @return the tail of this {@link AmortizedStack}
     */
    @Override
    public abstract AmortizedStack<A> tail();

    /**
     * Reverse this {@link AmortizedStack}. <code>O(n)</code>.
     *
     * @return this {@link AmortizedStack}, reversed
     */
    @Override
    public AmortizedStack<A> reverse() {
        return foldLeft(AmortizedStack::cons, amortizedStack(), this);
    }

    /**
     * If this {@link AmortizedStack} is not empty, return the head element wrapped in {@link Maybe}. Otherwise, return
     * {@link Maybe#nothing()}. <code>O(1)</code>.
     *
     * @return {@link Maybe} the head element of this {@link AmortizedStack}
     */
    @Override
    public abstract Maybe<A> head();

    /**
     * A {@link Computed.Once once-computed} {@link Value} representing the {@link SizeInfo} of this
     * {@link AmortizedStack}. Amortized <code>O(1)</code>.
     */
    @Override
    public abstract Computed.Once<Finite<Natural>> sizeInfo();

    /**
     * Returns true if this {@link AmortizedStack} is empty; otherwise, returns false. <code>O(1)</code>.
     *
     * @return whether or not this {@link AmortizedStack} is empty
     */
    @Override
    public abstract boolean isEmpty();

    /**
     * {@inheritDoc}
     * <code>O(o)</code>.
     */
    @Override
    public AmortizedStack<A> consAll(Collection<Natural, A> other) {
        return (AmortizedStack<A>) Stack.super.consAll(other);
    }

    /**
     * Returns true if <code>other</code> is an {@link AmortizedStack} with exactly the same elements in the same order
     * as this {@link AmortizedStack}; otherwise, returns false. <code>O(n)</code>.
     *
     * @param other the reference object with which to compare
     * @return true if the compared to a value-equal {@link AmortizedStack}
     */
    @Override
    public final boolean equals(Object other) {
        return other instanceof AmortizedStack<?> &&
                equivalent(elementsInOrder(objectEquals()), this, downcast(other));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final Iterator<A> iterator() {
        return new Iterator<A>() {
            AmortizedStack<A> rest = AmortizedStack.this;

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
     * Provide a debug-friendly string representation of this {@link AmortizedStack}. <code>O(n)</code>
     *
     * @return the string representation of this {@link AmortizedStack}
     */
    @Override
    public final String toString() {
        StringBuilder body = new StringBuilder("AmortizedStack[");

        AmortizedStack<A> next = this;
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
     * Create a {@link AmortizedStack} of zero or more elements, with the elements queued for removal from left to
     * right.
     * <code>O(n)</code>.
     *
     * @param as  the elements to {@link AmortizedStack#cons(Object) cons} from back to front
     * @param <A> the element type
     * @return the {@link AmortizedStack}
     */
    @SafeVarargs
    public static <A> AmortizedStack<A> amortizedStack(A... as) {
        @SuppressWarnings("unchecked")
        AmortizedStack<A> result = (AmortizedStack<A>) Empty.INSTANCE;
        for (int i = as.length - 1; i >= 0; i--)
             result = result.cons(as[i]);
        return result;
    }

    private static final class Head<A> extends AmortizedStack<A> {

        private static final AtomicReferenceFieldUpdater<Head<?>, Finite<Natural>> SIZE_UPDATER =
                newUpdater(Downcast.<Class<Head<?>>, Class<?>>downcast(Head.class),
                           Downcast.<Class<Finite<Natural>>, Class<?>>downcast(Finite.class),
                           "size");

        private static final AtomicReferenceFieldUpdater<Head<?>, Integer> HASH_CODE_UPDATER =
                newUpdater(Downcast.<Class<Head<?>>, Class<?>>downcast(Head.class),
                           Integer.class,
                           "hashCode");

        private final A                 head;
        private final AmortizedStack<A> tail;

        @SuppressWarnings("unused") private volatile Finite<Natural> size;
        @SuppressWarnings("unused") private volatile Integer         hashCode;

        private Head(A head, AmortizedStack<A> tail) {
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
        public AmortizedStack<A> tail() {
            return tail;
        }

        @Override
        public Computed.Once<Finite<Natural>> sizeInfo() {
            return computedOnce(volatileField(this, SIZE_UPDATER),
                                () -> finite(foldLeft((s, __) -> s.inc(), (Natural) zero(), this)));
        }

        @Override
        public int hashCode() {
            return computedOnce(volatileField(this, HASH_CODE_UPDATER),
                                () -> hash(elementsInOrder(objectHashCode()), this))
                    .getOrCompute();
        }
    }

    private static final class Empty<A> extends AmortizedStack<A> {
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
        public AmortizedStack<A> tail() {
            return this;
        }

        @Override
        public Computed.Once<Finite<Natural>> sizeInfo() {
            return computedOnce(() -> just(finite(zero())), () -> finite(zero()));
        }

        @Override
        public int hashCode() {
            return 0;
        }
    }
}
