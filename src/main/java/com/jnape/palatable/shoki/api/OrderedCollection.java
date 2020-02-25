package com.jnape.palatable.shoki.api;

import com.jnape.palatable.shoki.SizeInfo;

import java.util.Iterator;
import java.util.Objects;

/**
 * A {@link Collection} that supports stable ordering of the contained elements.
 *
 * @param <Size> the known size {@link Number} type
 * @param <A>    the element type
 * @see Stack
 * @see Queue
 */
public interface OrderedCollection<Size extends Number, A> extends Collection<Size, A> {

    /**
     * Produce a new instance of this {@link OrderedCollection} with the elements in the reverse order.
     *
     * @return the reversed {@link OrderedCollection}
     */
    OrderedCollection<Size, A> reverse();

    /**
     * @inheritDoc
     */
    @Override
    OrderedCollection<Size, A> tail();

    /**
     * Determine if two {@link OrderedCollection}s have the same {@link SizeInfo}, and contain the same elements in the
     * same order. <code>O(n)</code>.
     *
     * @param xs   the first {@link OrderedCollection}
     * @param ys   the second {@link OrderedCollection}
     * @param <OC> the {@link OrderedCollection} subtype of the arguments
     * @return true if both {@link OrderedCollection}s are equal by the parameters above; false otherwise
     */
    static <OC extends OrderedCollection<?, ?>> boolean equals(OC xs, OC ys) {
        if (Objects.equals(xs.sizeInfo(), ys.sizeInfo())) {
            Iterator<?> xsIt = xs.iterator();
            Iterator<?> ysIt = ys.iterator();

            while (xsIt.hasNext())
                if (!Objects.equals(xsIt.next(), ysIt.next()))
                    return false;
            return true;
        }
        return false;
    }
}
