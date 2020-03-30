package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.HashMap;
import org.junit.Test;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.impl.HashMap.empty;
import static com.jnape.palatable.shoki.impl.HashMap.of;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class MapTest {

    //todo: delete pre-commit
    @Test
    public void sameEntriesDetritus() {
        assertTrue(Map.sameEntries(HashMap.of((k4, k23) -> k4 % 2 == k23 % 2,
                                              k4 -> k4.hashCode() % 2,
                                              tuple(1, true),
                                              tuple(2, false),
                                              tuple(3, false),
                                              tuple(4, true)), of(tuple(3, false),
                                                                  tuple(4, true)), objectEquals()));

        assertTrue(Map.sameEntries(of(tuple(3, false),
                                      tuple(4, true)), HashMap.of((k3, k22) -> k3 % 2 == k22 % 2,
                                                                  k3 -> k3.hashCode() % 2,
                                                                  tuple(1, true),
                                                                  tuple(2, false),
                                                                  tuple(3, false),
                                                                  tuple(4, true)), objectEquals()));

        assertFalse(Map.sameEntries(HashMap.of((k1, k21) -> k1 % 2 == k21 % 2, k1 -> k1.hashCode() % 2,
                                               tuple(4, true),
                                               tuple(3, false),
                                               tuple(2, false),
                                               tuple(1, true)), of(tuple(3, false),
                                                                   tuple(4, true)), objectEquals()));


        assertFalse(Map.sameEntries(HashMap.<Integer, Boolean>empty()
                                            .put(3, false)
                                            .put(4, true), HashMap.<Integer, Boolean>empty((k, k2) -> k % 2 == k2 % 2,
                                                                                           k -> k.hashCode() % 2)
                                            .put(4, true)
                                            .put(3, false)
                                            .put(2, false)
                                            .put(1, true), objectEquals()));
    }

    @Test
    public void sameEntries() {
        assertTrue(Map.sameEntries(empty(), empty(), objectEquals()));
        assertTrue(Map.sameEntries(empty().put(1, 1), empty().put(1, 1), objectEquals()));
        assertTrue(Map.sameEntries(empty().put(1, 1).put(2, 2).remove(2), empty().put(1, 1), objectEquals()));

        assertTrue(Map.sameEntries(of(tuple(1, true), tuple(2, false)), of(tuple(1, true), tuple(2, false)), (v1, v2) -> true));
    }

    @Test
    public void sameEntriesDoesNotProveSymmetry() {
        HashMap<Integer, Boolean> m1 = of(tuple(1, false),
                                          tuple(2, true));

        HashMap<Integer, Boolean> m2 = HashMap.of((k1, k2) -> k1 % 2 == k2 % 2,
                                                  k -> k.hashCode() % 2,
                                                  tuple(3, false),
                                                  tuple(4, true));
        assertTrue(Map.sameEntries(m1, m2, objectEquals()));
        assertFalse(Map.sameEntries(m2, m1, objectEquals()));
    }

}