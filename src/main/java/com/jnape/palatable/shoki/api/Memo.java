package com.jnape.palatable.shoki.api;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

public interface Memo<A> {

    A getOrElse(A ifMissing);

    void set(A a);

    Object lock();

    static <Instance, A> Memo<A> updater(Instance instance, AtomicReferenceFieldUpdater<Instance, A> updater) {
        return new Updater<>(instance, updater);
    }

    final class Updater<Instance, A> implements Memo<A> {
        private final Instance                                 instance;
        @SuppressWarnings("AtomicFieldUpdaterNotStaticFinal")
        private final AtomicReferenceFieldUpdater<Instance, A> updater;

        private Updater(Instance instance, AtomicReferenceFieldUpdater<Instance, A> updater) {
            this.instance = instance;
            this.updater  = updater;
        }

        public A getOrElse(A ifMissing) {
            A a = updater.get(instance);
            return a == null ? ifMissing : a;
        }

        public void set(A a) {
            updater.set(instance, a);
        }

        public Instance lock() {
            return instance;
        }
    }
}
