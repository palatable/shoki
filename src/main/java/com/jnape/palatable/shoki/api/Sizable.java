package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.SizeInfo;

/**
 * A generic interface representing a type that can provide information about its size.
 *
 * @see Collection
 */
public interface Sizable {

    /**
     * Returns a {@link SizeInfo} representing any information this type has about its size.
     *
     * @return the information about this type's size
     */
    SizeInfo sizeInfo();
}
