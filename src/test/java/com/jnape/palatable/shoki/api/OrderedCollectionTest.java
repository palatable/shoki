package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.impl.StrictStack;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

@RunWith(Enclosed.class)
public class OrderedCollectionTest {

    public static final class EquivalenceRelations {

        @Test
        public void sameElementsSameOrder() {
            EquivalenceRelation<? super OrderedCollection<?, Integer>> sameElementsSameOrder =
                    OrderedCollection.EquivalenceRelations.sameElementsSameOrder(objectEquals());

            OrderedCollection<?, Integer> stack = StrictStack.of(1, 2, 3);
            OrderedCollection<?, Integer> queue = StrictQueue.of(3, 2, 1);

            assertThat(stack, equivalentTo(queue, sameElementsSameOrder));
            assertThat(queue, equivalentTo(stack, sameElementsSameOrder));
            assertThat(queue, equivalentTo(queue, sameElementsSameOrder));
            assertThat(stack, equivalentTo(stack, sameElementsSameOrder));

            assertThat(stack.reverse(), not(equivalentTo(stack, sameElementsSameOrder)));
            assertThat(stack, not(equivalentTo(stack.reverse(), sameElementsSameOrder)));
            assertThat(queue, not(equivalentTo(stack.reverse(), sameElementsSameOrder)));
            assertThat(stack.reverse(), not(equivalentTo(queue, sameElementsSameOrder)));
            assertThat(queue.reverse(), not(equivalentTo(queue, sameElementsSameOrder)));
            assertThat(queue, not(equivalentTo(queue.reverse(), sameElementsSameOrder)));
            assertThat(stack, not(equivalentTo(queue.reverse(), sameElementsSameOrder)));
            assertThat(queue.reverse(), not(equivalentTo(stack, sameElementsSameOrder)));
            assertThat(queue.reverse(), not(equivalentTo(StrictQueue.empty(), sameElementsSameOrder)));
        }
    }
}