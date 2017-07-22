package com.jnape.palatable.shoki;

import org.junit.Test;

import static com.jnape.palatable.shoki.Stream.$nil;
import static com.jnape.palatable.shoki.Stream.cons;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

public class StreamTest {

    @Test
    public void nilUsesSingleInstance() {
        assertSame(Stream.nil(), Stream.nil());
    }

    @Test
    public void coproduct() {
        Stream<Object> nil = Stream.nil();
        assertEquals(nil, nil.match(n -> n, c -> {
            throw new AssertionError();
        }));

        Stream<Integer> cons = cons(1, $nil());
        assertEquals(cons, cons.match(n -> {
                                          throw new AssertionError();
                                      },
                                      c -> c));
    }

    @Test
    public void destructuringCons() {
        Stream.Cons<Integer> cons = cons(1, $nil());
        assertEquals((Integer) 1, cons.head());
        assertEquals(Stream.nil(), cons.tail().force());
    }

    @Test
    public void fmap() {
        assertEquals((Integer) 2, cons(1, $nil()).fmap(x -> x + 1).head());
    }
}