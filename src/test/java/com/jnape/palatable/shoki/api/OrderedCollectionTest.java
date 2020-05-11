package com.jnape.palatable.shoki.api;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static com.jnape.palatable.shoki.testsupport.HashingAlgorithmMatcher.hashesEquivalentlyTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

@RunWith(Enclosed.class)
public class OrderedCollectionTest {

    public static final class EquivalenceRelations {

        @Test
        public void sameElementsSameOrder() {
            EquivalenceRelation<? super OrderedCollection<?, Integer>> sameElementsSameOrder =
                    OrderedCollection.EquivalenceRelations.elementsInOrder(objectEquals());

            OrderedCollection<?, Integer> stack = strictStack(1, 2, 3);
            OrderedCollection<?, Integer> queue = strictQueue(1, 2, 3);

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
            assertThat(queue.reverse(), not(equivalentTo(strictQueue(), sameElementsSameOrder)));
        }
    }

    public static final class HashingAlgorithms {

        @Test
        public void elementsInOrder() {
            HashingAlgorithm<? super OrderedCollection<?, Integer>> elementsInOrder =
                    OrderedCollection.HashingAlgorithms.elementsInOrder(objectHashCode());

            OrderedCollection<?, Integer> stack = strictStack(1, 2, 3);
            OrderedCollection<?, Integer> queue = strictQueue(1, 2, 3);

            assertThat(stack, hashesEquivalentlyTo(queue, elementsInOrder));
            assertThat(queue, hashesEquivalentlyTo(stack, elementsInOrder));
            assertThat(queue, hashesEquivalentlyTo(queue, elementsInOrder));
            assertThat(stack, hashesEquivalentlyTo(stack, elementsInOrder));

            assertThat(stack.reverse(), not(hashesEquivalentlyTo(stack, elementsInOrder)));
            assertThat(stack, not(hashesEquivalentlyTo(stack.reverse(), elementsInOrder)));
            assertThat(queue, not(hashesEquivalentlyTo(stack.reverse(), elementsInOrder)));
            assertThat(stack.reverse(), not(hashesEquivalentlyTo(queue, elementsInOrder)));
            assertThat(queue.reverse(), not(hashesEquivalentlyTo(queue, elementsInOrder)));
            assertThat(queue, not(hashesEquivalentlyTo(queue.reverse(), elementsInOrder)));
            assertThat(stack, not(hashesEquivalentlyTo(queue.reverse(), elementsInOrder)));
            assertThat(queue.reverse(), not(hashesEquivalentlyTo(stack, elementsInOrder)));
            assertThat(queue.reverse(), not(hashesEquivalentlyTo(strictQueue(), elementsInOrder)));
        }
    }
}