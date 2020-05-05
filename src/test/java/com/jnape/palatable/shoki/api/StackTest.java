package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.impl.StrictStack;
import com.jnape.palatable.shoki.testsupport.DefaultMethodsStack;
import org.junit.Test;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.sameElementsSameOrder;
import static com.jnape.palatable.shoki.impl.StrictStack.empty;
import static com.jnape.palatable.shoki.testsupport.EquivalenceRelationMatcher.equivalentTo;
import static org.junit.Assert.assertThat;

public class StackTest {

    @Test
    public void consAll() {
        assertThat(DefaultMethodsStack.delegate(empty()).consAll(StrictStack.of(1, 2, 3)),
                   equivalentTo(StrictStack.of(3, 2, 1), sameElementsSameOrder(objectEquals())));
        assertThat(DefaultMethodsStack.delegate(StrictStack.of(4, 5)).consAll(StrictStack.of(3, 2, 1)),
                   equivalentTo(StrictStack.of(1, 2, 3, 4, 5), sameElementsSameOrder(objectEquals())));
        assertThat(DefaultMethodsStack.delegate(StrictStack.of(5, 4)).consAll(StrictStack.empty()),
                   equivalentTo(StrictStack.of(5, 4), sameElementsSameOrder(objectEquals())));
    }
}