package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.testsupport.DefaultMethodsStack;
import org.junit.Test;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.elementsInOrder;
import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.junit.Assert.assertThat;

public class StackTest {

    @Test
    public void consAll() {
        assertThat(DefaultMethodsStack.delegate(strictStack()).consAll(strictStack(1, 2, 3)),
                   equivalentTo(strictStack(3, 2, 1), elementsInOrder(objectEquals())));

        assertThat(DefaultMethodsStack.delegate(strictStack(4, 5)).consAll(strictStack(3, 2, 1)),
                   equivalentTo(strictStack(1, 2, 3, 4, 5), elementsInOrder(objectEquals())));

        assertThat(DefaultMethodsStack.delegate(strictStack(4, 5)).consAll(strictStack()),
                   equivalentTo(strictStack(4, 5), elementsInOrder(objectEquals())));
    }
}