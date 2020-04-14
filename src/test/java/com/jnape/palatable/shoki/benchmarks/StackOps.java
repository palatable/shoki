package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.api.Stack;

import java.util.ArrayList;

public final class StackOps {

    public static Stack<?, Integer> consRangeShoki(Stack<?, Integer> stack, int upperBound) {
        for (int i = 0; i < upperBound; i++) {
            stack = stack.cons(i);
        }
        return stack;
    }

    public static java.util.Deque<Integer> consRangeJDK(java.util.Deque<Integer> deque, int upperBound) {
        for (int i = 0; i < upperBound; i++) {
            deque.addFirst(i);
        }
        return deque;
    }

    public static ArrayList<Integer> consRangeJDK(ArrayList<Integer> arrayList, int upperBound) {
        for (int i = 0; i < upperBound; i++) {
            arrayList.add(0, i);
        }
        return arrayList;

    }
}