package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.testsupport.DefaultMethodsQueue;
import org.junit.Test;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.elementsInOrder;
import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.junit.Assert.assertThat;

public class QueueTest {

    @Test
    public void snocAll() {
        assertThat(DefaultMethodsQueue.delegate(strictQueue()).snocAll(strictQueue(1, 2, 3)),
                   equivalentTo(strictQueue(1, 2, 3), elementsInOrder(objectEquals())));

        assertThat(DefaultMethodsQueue.delegate(strictQueue(-1, 0)).snocAll(strictQueue(1, 2, 3)),
                   equivalentTo(strictQueue(-1, 0, 1, 2, 3), elementsInOrder(objectEquals())));

        assertThat(DefaultMethodsQueue.delegate(strictQueue(-1, 0)).snocAll(strictQueue()),
                   equivalentTo(strictQueue(-1, 0), elementsInOrder(objectEquals())));
    }
}