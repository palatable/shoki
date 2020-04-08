package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictQueue;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsQueue;
import org.junit.Test;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.sameElementsSameOrder;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.junit.Assert.assertThat;

public class QueueTest {

    @Test
    public void snocAll() {
        assertThat(DefaultMethodsQueue.delegate(StrictQueue.empty()).snocAll(StrictQueue.of(1, 2, 3)),
                   equivalentTo(StrictQueue.of(1, 2, 3), sameElementsSameOrder(objectEquals())));

        assertThat(DefaultMethodsQueue.delegate(StrictQueue.of(-1, 0)).snocAll(StrictQueue.of(1, 2, 3)),
                   equivalentTo(StrictQueue.of(-1, 0, 1, 2, 3), sameElementsSameOrder(objectEquals())));

        assertThat(DefaultMethodsQueue.delegate(StrictQueue.of(-1, 0)).snocAll(StrictQueue.empty()),
                   equivalentTo(StrictQueue.of(-1, 0), sameElementsSameOrder(objectEquals())));
    }
}