package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.shoki.api.Collection;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.SizeInfo;
import com.jnape.palatable.shoki.api.SizeInfo.Known;
import com.jnape.palatable.shoki.api.Stack;
import com.jnape.palatable.shoki.impl.SizeAndHashCodePreference.Deferred;

import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.hash;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.elementsInOrder;
import static com.jnape.palatable.shoki.api.OrderedCollection.HashingAlgorithms.elementsInOrder;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static java.lang.String.join;

/**
 * A strictly-evaluated {@link Stack}.
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
    public abstract StrictStack<A> reverse();

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
    public final StrictStack<A> consAll(Collection<Natural, A> other) {
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
    public final String toString() {
        return "StrictStack[" + join(", ", map(Object::toString, this)) + "]";
    }

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
        return immediate(as);
    }

    @SafeVarargs
    public static <A, P extends SizeAndHashCodePreference> StrictStack<A>.WithSize<P> strictStack(
            P sizeAndHashCodePreference,
            A... as) {


        return null;
    }

    @SafeVarargs
    public static <A> EagerSizeAndHashCode<A> immediate(A... as) {
        EagerSizeAndHashCode<A> result = EagerSizeAndHashCode.nil();
        for (int i = as.length - 1; i >= 0; i--)
             result = result.cons(as[i]);
        return result;
    }

    @SafeVarargs
    public static <A> LazySizeAndHashCode<A> fastestWithDeferral(A... as) {
        LazySizeAndHashCode<A> result = LazySizeAndHashCode.nil();
        for (int i = as.length - 1; i >= 0; i--)
             result = result.cons(as[i]);
        return result;
    }

    public final WithSize<SizeAndHashCodePreference.Retained> retainSizeAndHashCode() {
        return null;
    }

    public final WithSize<Deferred> deferSizeAndHashCode() {
        return null;
    }

    public abstract class WithSize<P extends SizeAndHashCodePreference> extends StrictStack<A> {

        @Override
        public abstract WithSize<P> cons(A a);

        @Override
        public abstract WithSize<P> tail();

        @Override
        public abstract WithSize<P> reverse();
    }


    public static abstract class EagerSizeAndHashCode<A> extends StrictStack<A> {

        @Override
        public abstract EagerSizeAndHashCode<A> cons(A a);

        @Override
        public abstract EagerSizeAndHashCode<A> tail();

        @SuppressWarnings("unchecked")
        private static <A> Nil<A> nil() {
            return (Nil<A>) Nil.INSTANCE;
        }

        private static final class Nil<A> extends EagerSizeAndHashCode<A> {
            private static final Nil<?> INSTANCE = new Nil<>();

            private Nil() {
            }

            @Override
            public Cons<A> cons(A a) {
                return new Cons<>(a, LL.nil(), zero(), 0);
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
            public EagerSizeAndHashCode<A> tail() {
                return this;
            }

            @Override
            public Known<Natural> sizeInfo() {
                return known(zero());
            }

            @Override
            public Iterator<A> iterator() {
                return Collections.emptyIterator();
            }

            @Override
            public int hashCode() {
                return 0;
            }

            @Override
            public Nil<A> reverse() {
                return this;
            }
        }

        private static final class Cons<A> extends EagerSizeAndHashCode<A> {
            private final A       head;
            private final LL<A>   tail;
            private final Natural tailSize;
            private final int     tailHashCode;

            private Cons(A head, LL<A> tail, Natural tailSize, int tailHashCode) {
                this.head         = head;
                this.tail         = tail;
                this.tailSize     = tailSize;
                this.tailHashCode = tailHashCode;
            }

            @Override
            public Cons<A> cons(A a) {
                return new Cons<>(a,
                                  new LL.Cons<>(head, tail),
                                  tailSize.inc(),
                                  tailHashCode * 31 + Objects.hashCode(head));
            }

            @Override
            public EagerSizeAndHashCode<A> tail() {
                if (tail instanceof LL.Nil<?>) {
                    return nil();
                }

                LL.Cons<A> ne = (LL.Cons<A>) tail;
                return new Cons<>(ne.a,
                                  ne.as,
                                  tailSize.decOrZero(),
                                  (tailHashCode - Objects.hashCode(ne.a)) / 31);
            }

            @Override
            public Maybe<A> head() {
                return just(head);
            }

            @Override
            public Known<Natural> sizeInfo() {
                return known(tailSize.inc());
            }

            @Override
            public boolean isEmpty() {
                return false;
            }

            @Override
            public Iterator<A> iterator() {
                return new Iterator<A>() {
                    private LL<A> ll = new LL.Cons<>(head, tail);

                    @Override
                    public boolean hasNext() {
                        return ll instanceof LL.Cons<?>;
                    }

                    @Override
                    public A next() {
                        if (!hasNext())
                            throw new NoSuchElementException();

                        LL.Cons<A> cons = (LL.Cons<A>) ll;
                        ll = cons.as;
                        return cons.a;
                    }
                };
            }

            @Override
            public int hashCode() {
                return tailHashCode * 31 + Objects.hashCode(head);
            }

            @Override
            public EagerSizeAndHashCode<A> reverse() {
                return foldLeft(EagerSizeAndHashCode::cons, (EagerSizeAndHashCode<A>) EagerSizeAndHashCode.<A>nil(), this);
            }
        }

        private static abstract class LL<A> {
            private LL() {
            }

            @SuppressWarnings("unchecked")
            private static <A> Nil<A> nil() {
                return (Nil<A>) Nil.INSTANCE;
            }

            private static final class Nil<A> extends LL<A> {
                private static final Nil<?> INSTANCE = new Nil<>();
            }

            private static final class Cons<A> extends LL<A> {
                private final A     a;
                private final LL<A> as;

                private Cons(A a, LL<A> as) {
                    this.a  = a;
                    this.as = as;
                }
            }
        }
    }

    public static abstract class LazySizeAndHashCode<A> extends StrictStack<A>
            implements CoProduct2<LazySizeAndHashCode.Nil<A>, LazySizeAndHashCode.Cons<A>, LazySizeAndHashCode<A>> {

        @Override
        public abstract LazySizeAndHashCode<A> cons(A a);

        @Override
        public abstract LazySizeAndHashCode<A> tail();

        @Override
        public abstract LazySizeAndHashCode<A> reverse();

        @SuppressWarnings("unchecked")
        private static <A> Nil<A> nil() {
            return (Nil<A>) Nil.INSTANCE;
        }

        private static final class Nil<A> extends LazySizeAndHashCode<A> {
            private static final Nil<?> INSTANCE = new Nil<>();

            private Nil() {
            }

            @Override
            public Cons<A> cons(A a) {
                return new Cons<>(a, this);
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
            public Nil<A> tail() {
                return this;
            }

            @Override
            public Nil<A> reverse() {
                return this;
            }

            @Override
            public Known<Natural> sizeInfo() {
                return known(zero());
            }

            @Override
            public Iterator<A> iterator() {
                return Collections.emptyIterator();
            }

            @Override
            public int hashCode() {
                return 0;
            }

            @Override
            public <R> R match(Fn1<? super Nil<A>, ? extends R> aFn, Fn1<? super Cons<A>, ? extends R> bFn) {
                return aFn.apply(this);
            }
        }

        private static final class Cons<A> extends LazySizeAndHashCode<A> {
            private final A                      head;
            private final LazySizeAndHashCode<A> tail;

            private volatile Natural size;
            private volatile Integer hashCode;

            private Cons(A head, LazySizeAndHashCode<A> tail) {
                this.head = head;
                this.tail = tail;
            }

            @Override
            public Cons<A> cons(A a) {
                return new Cons<>(a, this);
            }

            @Override
            public Iterator<A> iterator() {
                return new Iterator<A>() {
                    private LazySizeAndHashCode<A> rest = Cons.this;

                    @Override
                    public boolean hasNext() {
                        return rest instanceof LazySizeAndHashCode.Cons;
                    }

                    @Override
                    public A next() {
                        if (!hasNext())
                            throw new NoSuchElementException();

                        Cons<A> headRest = (Cons<A>) this.rest;
                        rest = headRest.tail;
                        return headRest.head;
                    }
                };
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
            public LazySizeAndHashCode<A> tail() {
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
                            this.size = size = foldLeft((s, __) -> s.inc(), (Natural) zero(), this);
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
                            this.hashCode = hashCode = hash(elementsInOrder(objectHashCode()), this);
                        }
                    }
                }
                return hashCode;
            }

            @Override
            public LazySizeAndHashCode<A> reverse() {
                return foldLeft(LazySizeAndHashCode::cons, (LazySizeAndHashCode<A>) LazySizeAndHashCode.<A>nil(), this);
            }

            @Override
            public <R> R match(Fn1<? super Nil<A>, ? extends R> aFn, Fn1<? super Cons<A>, ? extends R> bFn) {
                return bFn.apply(this);
            }
        }
    }
}
