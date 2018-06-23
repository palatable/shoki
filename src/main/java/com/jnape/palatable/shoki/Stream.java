package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functor.Functor;

import java.util.function.Function;

import static com.jnape.palatable.shoki.$.$;

public abstract class Stream<A> implements Functor<A, Stream<?>>, CoProduct2<Stream.Nil<A>, Stream.Cons<A>, Stream<A>> {

    private Stream() {
    }

    @Override
    public abstract <B> Stream<B> fmap(Function<? super A, ? extends B> fn);

    @SuppressWarnings("unchecked")
    public static <A> Stream.Nil<A> nil() {
        return Nil.INSTANCE;
    }

    @SuppressWarnings("unchecked")
    public static <A> Stream.Cons<A> cons(A a, $<? extends Stream<A>> tail) {
        return new Cons<>(a, tail);
    }

    public static <A> $<Stream.Nil<A>> $nil() {
        return $(Stream::nil);
    }

    public static <A> $<Stream.Cons<A>> $cons(A a, $<? extends Stream<A>> tail) {
        return $(() -> cons(a, tail));
    }

    public static final class Nil<A> extends Stream<A> {
        private static final Nil INSTANCE = new Nil();

        private Nil() {
        }

        @Override
        @SuppressWarnings("unchecked")
        public <B> Stream.Nil<B> fmap(Function<? super A, ? extends B> fn) {
            return (Stream.Nil<B>) this;
        }

        @Override
        public <R> R match(Function<? super Nil<A>, ? extends R> aFn, Function<? super Cons<A>, ? extends R> bFn) {
            return aFn.apply(this);
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
        public <B> Stream.Cons<B> fmap(Function<? super A, ? extends B> fn) {
            return new Cons<>(fn.apply(a), tail.fmap(s -> s.fmap(fn)));
        }

        @Override
        public <R> R match(Function<? super Nil<A>, ? extends R> aFn, Function<? super Cons<A>, ? extends R> bFn) {
            return bFn.apply(this);
        }
    }
}
