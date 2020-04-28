package com.jnape.palatable.shoki.testsupport;

import com.jnape.palatable.shoki.api.HashingAlgorithm;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

import java.util.Objects;

import static java.lang.String.format;

/**
 * A {@link org.hamcrest.Matcher matcher} that matches successfully if an expected value and actual value both produce
 * the same hash code according to the provided {@link HashingAlgorithm}.
 *
 * @param <A> the value type
 */
public final class HashingAlgorithmMatcher<A> extends TypeSafeMatcher<A> {

    private final A                           expected;
    private final HashingAlgorithm<? super A> hashingAlgorithm;

    private HashingAlgorithmMatcher(A expected, HashingAlgorithm<? super A> hashingAlgorithm) {
        this.expected         = expected;
        this.hashingAlgorithm = hashingAlgorithm;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean matchesSafely(A a) {
        return Objects.equals(hashingAlgorithm.apply(expected),
                              hashingAlgorithm.apply(a));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void describeTo(Description description) {
        description.appendText(format("Same hash as <%s> (%s) in terms of the given hashing algorithm",
                                      expected,
                                      hashingAlgorithm.apply(expected)));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void describeMismatchSafely(A item, Description mismatchDescription) {
        mismatchDescription.appendText("was ").appendValue(hashingAlgorithm.apply(item))
                .appendText(" (").appendValue(item).appendText(")");
    }

    /**
     * Static factory method for creating an {@link HashingAlgorithmMatcher} given an <code>expected</code> value
     * and a {@link HashingAlgorithm}.
     *
     * @param expected         the expected value
     * @param hashingAlgorithm the {@link HashingAlgorithm}
     * @param <A>              the value type
     * @return the {@link HashingAlgorithmMatcher}
     */
    public static <A> HashingAlgorithmMatcher<A> hashesEquivalentlyTo(A expected,
                                                                      HashingAlgorithm<? super A> hashingAlgorithm) {
        return new HashingAlgorithmMatcher<>(expected, hashingAlgorithm);
    }
}
