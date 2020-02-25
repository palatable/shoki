package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.SizeInfo.Known;
import com.jnape.palatable.shoki.api.OrderedCollection;
import com.jnape.palatable.shoki.api.Stack;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.SizeInfo.known;
import static java.util.Arrays.asList;

/**
 * An immutable, structure-sharing implementation of {@link Stack}.
 *
 * @param <A> the element type
 */
public abstract class ImmutableStack<A> implements Stack<Integer, A> {

    private ImmutableStack() {
    }

    /**
     * If this {@link ImmutableStack} is not empty, return a {@link Tuple2} of the head and tail wrapped in {@link
     * Maybe}. Otherwise, return {@link Maybe#nothing()}. <code>O(1)</code>.
     *
     * @return {@link Maybe} the head and tail of this {@link ImmutableStack}
     */
    public abstract Maybe<Tuple2<A, ImmutableStack<A>>> pop();

    /**
     * Produce a new {@link ImmutableStack} instance with <code>a</code> added to the front. <code>O(1)</code>.
     *
     * @param a the element
     * @return the new {@link ImmutableStack}
     */
    @Override
    public final ImmutableStack<A> cons(A a) {
        return new Head<>(a, this);
    }

    /**
     * The remaining elements after removing the head of this {@link ImmutableStack}, or {@link ImmutableStack#empty()}
     * if there are no elements. <code>O(1)</code>.
     *
     * @return the tail of this {@link ImmutableStack}
     */
    @Override
    public abstract ImmutableStack<A> tail();

    /**
     * Reverse this {@link ImmutableStack}. <code>O(n)</code>.
     *
     * @return this {@link ImmutableStack}, reversed
     */
    @Override
    public ImmutableStack<A> reverse() {
        return foldLeft(ImmutableStack::cons, ImmutableStack.empty(), this);
    }

    /**
     * If this {@link ImmutableStack} is not empty, return the head element wrapped in {@link Maybe}. Otherwise, return
     * {@link Maybe#nothing()}. <code>O(1)</code>.
     *
     * @return {@link Maybe} the head element of this {@link ImmutableStack}
     */
    @Override
    public abstract Maybe<A> head();

    /**
     * The {@link SizeInfo} of this {@link ImmutableStack}. <code>O(1)</code>.
     */
    @Override
    public abstract Known<Integer> sizeInfo();

    /**
     * Returns true if this {@link ImmutableStack} is empty; otherwise, returns false. <code>O(1)</code>.
     *
     * @return whether or not this {@link ImmutableStack} is empty
     */
    @Override
    public abstract boolean isEmpty();

    /**
     * Returns true if <code>other</code> is an {@link ImmutableStack} with exactly the same elements in the same order
     * as this {@link ImmutableStack}; otherwise, returns false. <code>O(n)</code>.
     *
     * @param other the reference object with which to compare
     * @return true if the compared to a value-equal {@link ImmutableStack}
     */
    @Override
    public final boolean equals(Object other) {
        return other instanceof ImmutableStack && OrderedCollection.equals(this, (ImmutableStack<?>) other);
    }

    /**
     * Provide a debug-friendly string representation of this {@link ImmutableStack}. <code>O(n)</code>
     *
     * @return the string representation of this {@link ImmutableStack}
     */
    @Override
    public final String toString() {
        StringBuilder body = new StringBuilder("ImmutableStack[");

        ImmutableStack<A> next = this;
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
     * The empty singleton instance of this {@link ImmutableStack}.
     *
     * @param <A> the {@link ImmutableStack} element type
     * @return an empty stack
     */
    @SuppressWarnings("unchecked")
    public static <A> ImmutableStack<A> empty() {
        return (ImmutableStack<A>) Empty.INSTANCE;
    }

    /**
     * Convenience static factory method to construct an {@link ImmutableStack} from an {@link Iterable} of elements.
     * <code>O(n)</code>.
     *
     * @param as  the {@link Iterable} of elements from back to front
     * @param <A> the {@link Iterable} and {@link ImmutableStack} element type
     * @return the new {@link ImmutableStack}
     */
    public static <A> ImmutableStack<A> of(Iterable<A> as) {
        return foldLeft(ImmutableStack::cons, empty(), as);
    }

    /**
     * Convenience static factory method to construct an {@link ImmutableStack} from varargs elements.
     * <code>O(n)</code>.
     *
     * @param as  the elements from back to front
     * @param <A> the {@link ImmutableStack} element type
     * @return the new {@link ImmutableStack}
     */
    @SafeVarargs
    public static <A> ImmutableStack<A> of(A... as) {
        return of(asList(as));
    }

    private static final class Head<A> extends ImmutableStack<A> {
        private final A                 head;
        private final ImmutableStack<A> tail;
        private final Known<Integer>    sizeInfo;
        private final int               hashCode;

        private Head(A head, ImmutableStack<A> tail) {
            this.head = head;
            this.tail = tail;
            sizeInfo = known(tail.sizeInfo().getSize() + 1);
            hashCode = tail.hashCode() * 31 + Objects.hash(head);
        }

        @Override
        public Maybe<Tuple2<A, ImmutableStack<A>>> pop() {
            return just(tuple(head, tail));
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
        public ImmutableStack<A> tail() {
            return tail;
        }

        @Override
        public Known<Integer> sizeInfo() {
            return sizeInfo;
        }

        @Override
        public int hashCode() {
            return hashCode;
        }
    }

    private static final class Empty<A> extends ImmutableStack<A> {
        private static final Empty<?> INSTANCE = new Empty<>();

        private Empty() {
        }

        @Override
        public Maybe<Tuple2<A, ImmutableStack<A>>> pop() {
            return nothing();
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
        public ImmutableStack<A> tail() {
            return this;
        }

        @Override
        public Known<Integer> sizeInfo() {
            return known(0);
        }
    }
}
