package com.jnape.palatable.shoki.benchmarks;

import com.jnape.palatable.shoki.api.Queue;

import java.util.ArrayList;

public final class QueueOps {

    public static Queue<?, Integer> snocRangeShoki(Queue<?, Integer> stack, int upperBound) {
        for (int i = 0; i < upperBound; i++) {
            stack = stack.snoc(i);
        }
        return stack;
    }

    public static java.util.Deque<Integer> snocRangeJDK(java.util.Deque<Integer> deque, int upperBound) {
        for (int i = 0; i < upperBound; i++) {
            deque.addLast(i);
        }
        return deque;
    }

    public static ArrayList<Integer> snocRangeJDK(ArrayList<Integer> arrayList, int upperBound) {
        for (int i = 0; i < upperBound; i++) {
            arrayList.add(i);
        }
        return arrayList;

    }
}