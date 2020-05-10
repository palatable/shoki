package com.jnape.palatable.shoki.api;

import com.jnape.palatable.lambda.functions.Fn1;
import com.jnape.palatable.traitor.annotations.TestTraits;
import com.jnape.palatable.traitor.runners.Traits;
import org.junit.Test;
import org.junit.runner.RunWith;
import testsupport.traits.ApplicativeLaws;
import testsupport.traits.Equivalence;
import testsupport.traits.FunctorLaws;
import testsupport.traits.MonadLaws;
import testsupport.traits.MonadReaderLaws;
import testsupport.traits.MonadRecLaws;
import testsupport.traits.MonadWriterLaws;

import java.util.Objects;

import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.hash;
import static org.junit.Assert.assertEquals;
import static testsupport.traits.Equivalence.equivalence;

@RunWith(Traits.class)
public class HashingAlgorithmTest {

    @TestTraits({FunctorLaws.class,
                 ApplicativeLaws.class,
                 MonadLaws.class,
                 MonadRecLaws.class,
                 MonadReaderLaws.class,
                 MonadWriterLaws.class})
    public Equivalence<? super Fn1<String, Object>> testSubject() {
        return equivalence(HashingAlgorithm.<String>objectHashCode(), h -> h.apply("foo"));
    }

    @Test
    public void objectHashCode() {
        HashingAlgorithm<Object> objectHashCode = HashingAlgorithm.objectHashCode();
        Object                   obj            = new Object();
        assertEquals((Integer) Objects.hashCode(obj), objectHashCode.apply(obj));
    }

    @Test
    public void identityHashCode() {
        HashingAlgorithm<Object> identityHashCode = HashingAlgorithm.identityHashCode();
        Object                   obj              = new Object();
        assertEquals((Integer) System.identityHashCode(obj), identityHashCode.apply(obj));
    }

    @Test
    public void hashing() {
        assertEquals(0, hash(0, HashingAlgorithm.objectHashCode()));
        assertEquals(1, hash(1, HashingAlgorithm.objectHashCode()));
    }

    @Test
    public void discardR() {
        assertEquals((Integer) 2, HashingAlgorithm.objectHashCode().discardR(constantly(1)).apply(2));
    }
}