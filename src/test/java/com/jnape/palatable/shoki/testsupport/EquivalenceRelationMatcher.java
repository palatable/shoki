package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.shoki.api.EquivalenceRelation;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

import static java.lang.String.format;

/**
 * A {@link org.hamcrest.Matcher matcher} that matches successfully if an expected value and actual value are
 * {@link EquivalenceRelation#equivalent(EquivalenceRelation, Object, Object) equivalent} in terms of the provided
 * {@link EquivalenceRelation}.
 *
 * @param <A> the value type
 */
public final class EquivalenceRelationMatcher<A> extends TypeSafeMatcher<A> {

    private final A                              expected;
    private final EquivalenceRelation<? super A> equivalenceRelation;

    private EquivalenceRelationMatcher(A expected, EquivalenceRelation<? super A> equivalenceRelation) {
        this.expected            = expected;
        this.equivalenceRelation = equivalenceRelation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean matchesSafely(A a) {
        return equivalenceRelation.apply(expected, a);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void describeTo(Description description) {
        description.appendText(format("Equivalence with <%s> in terms of the given equivalence relation", expected));
    }

    /**
     * Static factory method for creating an {@link EquivalenceRelationMatcher} given an <code>expected</code> value
     * and an {@link EquivalenceRelation}.
     *
     * @param expected            the expected value
     * @param equivalenceRelation the {@link EquivalenceRelation}
     * @param <A>                 the value type
     * @return the {@link EquivalenceRelationMatcher}
     */
    public static <A> EquivalenceRelationMatcher<A> equivalentTo(A expected,
                                                                 EquivalenceRelation<? super A> equivalenceRelation) {
        return new EquivalenceRelationMatcher<>(expected, equivalenceRelation);
    }
}
