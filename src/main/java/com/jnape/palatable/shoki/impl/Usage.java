package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.impl.SizeAndHashCodePreference.Deferred;

import static com.jnape.palatable.shoki.impl.SizeAndHashCodePreference.deferred;

public class Usage {

    public static void main(String[] args) {


        StrictStack<Integer>.WithSize<Deferred> integers = StrictStack.strictStack(deferred(), 1, 2, 3);

        StrictStack<Integer>.WithSize<Deferred> ss = null;

        StrictStack<Integer> cons = ss.cons(1).cons(3);

        StrictStack<Integer>.WithSize<Deferred> cons2 =
                cons.cons(4)
                        .retainSizeAndHashCode()
                        .cons(3)
                        .cons(4)
                        .deferSizeAndHashCode();



    }
}
