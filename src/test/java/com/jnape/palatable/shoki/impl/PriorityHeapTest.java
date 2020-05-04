package com.jnape.palatable.shoki.impl;

import org.junit.Test;

import java.util.Collections;
import java.util.Comparator;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.shoki.api.Natural.abs;
import static com.jnape.palatable.shoki.api.Natural.one;
import static com.jnape.palatable.shoki.api.Natural.zero;
import static com.jnape.palatable.shoki.api.SizeInfo.known;
import static java.util.Comparator.comparing;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.reverseOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThat;
import static testsupport.matchers.IterableMatcher.isEmpty;
import static testsupport.matchers.IterableMatcher.iterates;

public class PriorityHeapTest {

    @Test
    public void insertionAndHead() {
        PriorityHeap<Integer> empty = PriorityHeap.empty(naturalOrder());

        assertEquals(nothing(), empty.head());
        assertEquals(just(1), empty.insert(1).head());
        assertEquals(just(1), empty.insert(1).insert(2).head());
        assertEquals(just(1), empty.insert(2).insert(1).head());
        assertEquals(nothing(), empty.insert(1).tail().head());
        assertEquals(nothing(), empty.insert(1).tail().head());

        assertEquals(just(2), empty.insert(1).insert(2).tail().head());
        assertEquals(nothing(), empty.insert(1).insert(2).tail().tail().head());
    }

    @Test
    public void sizeInfo() {
        PriorityHeap<Integer> empty = PriorityHeap.empty(naturalOrder());

        assertEquals(known(zero()), empty.sizeInfo());
        assertEquals(known(zero()), empty.tail().sizeInfo());
        assertEquals(known(one()), empty.insert(1).sizeInfo());
        assertEquals(known(abs(2)), empty.insert(1).insert(2).sizeInfo());
        assertEquals(known(one()), empty.insert(1).insert(2).tail().sizeInfo());
    }

    @Test
    public void reverse() {
        PriorityHeap<Integer> empty = PriorityHeap.empty(naturalOrder());
        assertEquals(PriorityHeap.empty(naturalOrder().reversed()), empty.reverse());
        assertEquals(PriorityHeap.empty(Comparator.<Integer>naturalOrder().reversed()).insert(1),
                     empty.insert(1).reverse());
        assertEquals(PriorityHeap.empty(Comparator.<Integer>naturalOrder().reversed()).insert(1).insert(2),
                     empty.insert(1).insert(2).reverse());
        assertEquals(PriorityHeap.empty(Comparator.<Integer>naturalOrder().reversed()).insert(1).insert(2),
                     empty.insert(2).insert(1).reverse());
    }

    @Test
    public void prioritize() {
        Comparator<Integer>   comparator = reverseOrder();
        PriorityHeap<Integer> empty      = PriorityHeap.empty(naturalOrder());
        assertEquals(PriorityHeap.empty(comparator), empty.prioritize(comparator));
        assertEquals(PriorityHeap.empty(comparator).insert(1),
                     empty.insert(1).prioritize(comparator));
        assertEquals(PriorityHeap.empty(comparator).insert(1).insert(2),
                     empty.insert(1).insert(2).prioritize(comparator));
        assertEquals(PriorityHeap.empty(comparator).insert(1).insert(2),
                     empty.insert(2).insert(1).prioritize(comparator));
    }

    @Test
    public void equalsAndHashCode() {
        assertEquals(PriorityHeap.empty(naturalOrder()), PriorityHeap.empty(naturalOrder()));
        assertNotEquals(PriorityHeap.empty(naturalOrder()), PriorityHeap.empty(reverseOrder()));

        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1),
                     PriorityHeap.<Integer>empty(naturalOrder()).insert(1));
        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2),
                     PriorityHeap.<Integer>empty(naturalOrder()).insert(2).insert(1));

        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2),
                     PriorityHeap.<Integer>empty(naturalOrder()).insert(2).insert(1));

        assertNotEquals(PriorityHeap.<Integer>empty(naturalOrder()),
                        PriorityHeap.<Integer>empty(naturalOrder()).insert(1));
        assertNotEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1),
                        PriorityHeap.<Integer>empty(naturalOrder()).insert(2));
        assertNotEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1),
                        PriorityHeap.<Integer>empty(reverseOrder()).insert(1));

        assertEquals(PriorityHeap.<Integer>empty(reverseOrder()).insert(3).insert(2).insert(1),
                     PriorityHeap.<Integer>empty(naturalOrder()).insert(3).insert(2).insert(1).reverse());

        assertNotEquals(PriorityHeap.<Integer>empty(reverseOrder()), new Object());

        assertEquals(PriorityHeap.empty(naturalOrder()).hashCode(), PriorityHeap.empty(naturalOrder()).hashCode());
        assertEquals(PriorityHeap.empty(reverseOrder()).hashCode(), PriorityHeap.empty(naturalOrder()).hashCode());
        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).hashCode(),
                     PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).hashCode());

        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).hashCode(),
                     PriorityHeap.<Integer>empty(reverseOrder()).insert(1).hashCode());

        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).hashCode(),
                     PriorityHeap.<Integer>empty(reverseOrder()).insert(1).insert(2).reverse().hashCode());

        assertNotEquals(PriorityHeap.<Integer>empty(naturalOrder()).hashCode(),
                        PriorityHeap.<Integer>empty(naturalOrder()).insert(1).hashCode());

        assertNotEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).hashCode(),
                        PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).hashCode());

        assertNotEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).hashCode(),
                        PriorityHeap.<Integer>empty(reverseOrder()).insert(1).insert(2).hashCode());
    }

    @Test
    public void iteration() {
        assertThat(PriorityHeap.<Integer>empty(naturalOrder()), isEmpty());
        assertThat(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).insert(3),
                   iterates(1, 2, 3));
        assertThat(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).insert(3),
                   iterates(1, 2, 3));
        assertThat(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).insert(3).reverse(),
                   iterates(3, 2, 1));
        assertThat(PriorityHeap.<Integer>empty(naturalOrder()).reverse().insert(1).insert(2).reverse().insert(3).reverse(),
                   iterates(3, 2, 1));
    }

    @Test
    public void usefulToString() {
        assertEquals("Heap[]", PriorityHeap.<Integer>empty(naturalOrder()).toString());
        assertEquals("Heap[]", PriorityHeap.<Integer>empty(reverseOrder()).toString());

        assertEquals("Heap[1 ∘ 2 ∘ 3]",
                     PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).insert(3).toString());
        assertEquals("Heap[1 ∘ 2 ∘ 3]",
                     PriorityHeap.empty(Comparator.<Integer>naturalOrder().reversed().reversed())
                             .insert(1).insert(2).insert(3).toString());
        assertEquals("Heap[1 ∘ 2 ∘ 3]",
                     PriorityHeap.empty(Collections.<Integer>reverseOrder(naturalOrder())).reverse()
                             .insert(1).insert(2).insert(3).toString());

        assertEquals("Heap[3 ∘ 2 ∘ 1]",
                     PriorityHeap.<Integer>empty(reverseOrder()).insert(1).insert(2).insert(3).toString());
        assertEquals("Heap[3 ∘ 2 ∘ 1]",
                     PriorityHeap.empty(Comparator.<Integer>naturalOrder().reversed()).insert(1).insert(2).insert(3).toString());
        assertEquals("Heap[3 ∘ 2 ∘ 1]",
                     PriorityHeap.empty(Collections.<Integer>reverseOrder())
                             .insert(1).insert(2).insert(3).toString());
        assertEquals("Heap[3 ∘ 2 ∘ 1]",
                     PriorityHeap.empty(Collections.<Integer>reverseOrder(naturalOrder()))
                             .insert(1).insert(2).insert(3).toString());

        @SuppressWarnings("FunctionalExpressionCanBeFolded")
        Comparator<Integer> customNaturalOrder = Comparator.<Integer>naturalOrder()::compare;
        assertEquals("Heap[1 ∘ 2 ∘ 3]",
                     PriorityHeap.empty(customNaturalOrder).insert(1).insert(2).insert(3).toString());
    }

    @Test
    public void staticFactoryMethod() {
        assertEquals(PriorityHeap.empty(naturalOrder()), PriorityHeap.min());
        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).insert(3),
                     PriorityHeap.min(1, 2, 3));

        assertEquals(PriorityHeap.empty(reverseOrder()), PriorityHeap.max());
        assertEquals(PriorityHeap.<Integer>empty(reverseOrder()).insert(1).insert(2).insert(3),
                     PriorityHeap.max(1, 2, 3));

        assertEquals(PriorityHeap.<Integer>empty(naturalOrder()).insert(1).insert(2).insert(3),
                     PriorityHeap.of(naturalOrder(), 1, 2, 3));
        assertEquals(PriorityHeap.<Integer>empty(reverseOrder()).insert(1).insert(2).insert(3),
                     PriorityHeap.of(reverseOrder(), 1, 2, 3));

        class Item {
            String label;
        }
        Item             a              = new Item() {{ label = "a"; }};
        Item             b              = new Item() {{ label = "b"; }};
        Item             c              = new Item() {{ label = "c"; }};
        Comparator<Item> itemComparator = comparing(item -> item.label);
        assertEquals(PriorityHeap.empty(itemComparator).insert(a).insert(b).insert(c),
                     PriorityHeap.of(itemComparator, a, b, c));
    }
}