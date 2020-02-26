package com.jnape.palatable.shoki.api;

/**
 * An interface offering a mechanism for membership checking for a value of type <code>A</code>.
 *
 * @param <A> the value type
 * @see RandomAccess
 * @see Map
 * @see Set
 */
public interface Membership<A> {

    /**
     * Determine if some value <code>a</code> is a member of this type.
     *
     * @param a the value
     * @return true if a is a member; false otherwise
     */
    boolean contains(A a);
}
