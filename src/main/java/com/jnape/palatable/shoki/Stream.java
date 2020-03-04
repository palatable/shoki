package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.adt.hlist.HList;
import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.lambda.functor.Functor;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.recursion.RecursiveResult.recurse;
import static com.jnape.palatable.lambda.functions.recursion.RecursiveResult.terminate;
import static com.jnape.palatable.lambda.functions.recursion.Trampoline.trampoline;
import static com.jnape.palatable.shoki.$.$;

public abstract class Stream<A> implements Functor<A, Stream<?>>, CoProduct2<Stream.Nil<A>, Stream.Cons<A>, Stream<A>> {

    private Stream() {
    }

    @Override
    public abstract <B> Stream<B> fmap(Fn1<? super A, ? extends B> fn);

    @SuppressWarnings("unchecked")
    public static <A> Stream.Nil<A> nil() {
        return (Nil<A>) Nil.INSTANCE;
    }

    public static <A> Stream.Cons<A> cons(A a, $<? extends Stream<A>> tail) {
        return new Cons<>(a, tail);
    }

    @SuppressWarnings("unchecked")
    public static <A> $<Nil<A>> $nil() {
        return ($<Nil<A>>) (Object) Nil.$INSTANCE;
    }

    public static <A> $<Cons<A>> $cons(A a, $<? extends Stream<A>> tail) {
        return $(cons(a, tail));
    }

    public static final class Nil<A> extends Stream<A> {
        private static final Nil<?>    INSTANCE  = new Nil<>();
        private static final $<Nil<?>> $INSTANCE = $(INSTANCE);

        private Nil() {
        }

        @Override
        @SuppressWarnings("unchecked")
        public <B> Stream.Nil<B> fmap(Fn1<? super A, ? extends B> fn) {
            return (Stream.Nil<B>) this;
        }

        @Override
        public <R> R match(Fn1<? super Nil<A>, ? extends R> aFn, Fn1<? super Cons<A>, ? extends R> bFn) {
            return aFn.apply(this);
        }

        @Override
        public String toString() {
            return "Nil{}";
        }
    }

    public static final class Cons<A> extends Stream<A> {

        private final A                      a;
        private final $<? extends Stream<A>> tail;

        private Cons(A a, $<? extends Stream<A>> tail) {
            this.a = a;
            this.tail = tail;
        }

        public A head() {
            return a;
        }

        public $<? extends Stream<A>> tail() {
            return tail;
        }

        @Override
        public <B> Stream.Cons<B> fmap(Fn1<? super A, ? extends B> fn) {
            return new Cons<>(fn.apply(a), tail.fmap(s -> s.fmap(fn)));
        }

        @Override
        public <R> R match(Fn1<? super Nil<A>, ? extends R> aFn, Fn1<? super Cons<A>, ? extends R> bFn) {
            return bFn.apply(this);
        }

        @Override
        public String toString() {
            return "Cons{" +
                "a=" + a +
                ", tail=" + tail +
                '}';
        }
    }

    public static <A> $<? extends Stream<A>> reverse($<? extends Stream<A>> $stream) {
        return $(() -> trampoline(
            into(($s, r) -> $s.force().match(
                constantly(terminate(r)),
                cons -> recurse(tuple(cons.tail(), cons(cons.head(), $(r)))))),
            HList.<$<? extends Stream<A>>, Stream<A>>tuple($stream, nil())));
    }

    public static <A> $<? extends Stream<A>> concat($<? extends Stream<A>> $front, $<? extends Stream<A>> $back) {
        return $(() -> $front.force().match(__ -> $back.force(),
                                            cons -> cons(cons.head(), concat(cons.tail(), $back))));
    }
}
