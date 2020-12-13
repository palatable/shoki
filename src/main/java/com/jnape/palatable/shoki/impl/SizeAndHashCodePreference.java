package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.coproduct.CoProduct2;
import com.jnape.palatable.lambda.functions.Fn1;

public abstract class SizeAndHashCodePreference
        implements CoProduct2<
        SizeAndHashCodePreference.Retained,
        SizeAndHashCodePreference.Deferred,
        SizeAndHashCodePreference> {

    private SizeAndHashCodePreference() {
    }

    public static Retained retained() {
        return Retained.INSTANCE;
    }

    public static Deferred deferred() {
        return Deferred.INSTANCE;
    }

    public static final class Retained extends SizeAndHashCodePreference {
        private static final Retained INSTANCE = new Retained();

        private Retained() {
        }

        @Override
        public <R> R match(Fn1<? super Retained, ? extends R> aFn, Fn1<? super Deferred, ? extends R> bFn) {
            return aFn.apply(this);
        }
    }

    public static final class Deferred extends SizeAndHashCodePreference {
        private static final Deferred INSTANCE = new Deferred();

        private Deferred() {
        }

        @Override
        public <R> R match(Fn1<? super Retained, ? extends R> aFn, Fn1<? super Deferred, ? extends R> bFn) {
            return bFn.apply(this);
        }
    }
}
