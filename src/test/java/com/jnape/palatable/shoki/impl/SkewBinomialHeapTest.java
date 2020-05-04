package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.shoki.impl.SkewBinomialHeap.Node;
import org.junit.Test;

import java.util.Comparator;

import static com.jnape.palatable.lambda.adt.Maybe.just;
import static com.jnape.palatable.lambda.adt.Maybe.nothing;
import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.shoki.impl.StrictStack.empty;
import static com.jnape.palatable.shoki.impl.StrictStack.of;
import static java.util.Comparator.naturalOrder;
import static org.junit.Assert.assertEquals;

public class SkewBinomialHeapTest {

    private static final Comparator<Integer> C = naturalOrder();

    private static final Node<Integer> _0_0_EMPTY = new Node<>(0, 0, empty());
    private static final Node<Integer> _1_0_EMPTY = new Node<>(1, 0, empty());
    private static final Node<Integer> _2_0_EMPTY = new Node<>(2, 0, empty());
    private static final Node<Integer> _3_0_EMPTY = new Node<>(3, 0, empty());
    private static final Node<Integer> _4_0_EMPTY = new Node<>(4, 0, empty());
    private static final Node<Integer> _5_0_EMPTY = new Node<>(5, 0, empty());

    private static final Node<Integer> _0_1_0 = new Node<>(0, 1, of(_0_0_EMPTY));
    private static final Node<Integer> _1_1_1 = new Node<>(1, 1, of(_1_0_EMPTY));
    private static final Node<Integer> _2_1_2 = new Node<>(2, 1, of(_2_0_EMPTY));

    @Test
    public void link() {
        assertEquals(new Node<>(_0_0_EMPTY.value, _0_0_EMPTY.rank + 1, of(_1_0_EMPTY)),
                     SkewBinomialHeap.link(_0_0_EMPTY, _1_0_EMPTY, C));
        assertEquals(new Node<>(_0_0_EMPTY.value, _0_0_EMPTY.rank + 1, of(_1_0_EMPTY)),
                     SkewBinomialHeap.link(_1_0_EMPTY, _0_0_EMPTY, C));

        assertEquals(new Node<>(_0_0_EMPTY.value, _0_0_EMPTY.rank + 1, of(_0_0_EMPTY)),
                     SkewBinomialHeap.link(_0_0_EMPTY, _0_0_EMPTY, C));

        assertEquals(new Node<>(_0_1_0.value, _0_1_0.rank + 1, _0_1_0.children.cons(_1_1_1)),
                     SkewBinomialHeap.link(_0_1_0, _1_1_1, C));
        assertEquals(new Node<>(_0_1_0.value, _0_1_0.rank + 1, _0_1_0.children.cons(_1_1_1)),
                     SkewBinomialHeap.link(_1_1_1, _0_1_0, C));
    }

    @Test
    public void skewLink() {
        assertEquals(new Node<>(_0_0_EMPTY.value, _0_0_EMPTY.rank + 1,
                                of(_1_0_EMPTY, _2_0_EMPTY)),
                     SkewBinomialHeap.skewLink(_0_0_EMPTY, _1_0_EMPTY, _2_0_EMPTY, C));
        assertEquals(new Node<>(_0_0_EMPTY.value, _0_0_EMPTY.rank + 1,
                                of(_2_0_EMPTY, _1_0_EMPTY)),
                     SkewBinomialHeap.skewLink(_1_0_EMPTY, _0_0_EMPTY, _2_0_EMPTY, C));

        assertEquals(new Node<>(_0_0_EMPTY.value, _0_0_EMPTY.rank + 1,
                                of(_1_0_EMPTY, _2_0_EMPTY)),
                     SkewBinomialHeap.skewLink(_2_0_EMPTY, _1_0_EMPTY, _0_0_EMPTY, C));

        assertEquals(new Node<>(_0_0_EMPTY.value, _0_0_EMPTY.rank + 1,
                                of(_1_0_EMPTY, _0_0_EMPTY)),
                     SkewBinomialHeap.skewLink(_0_0_EMPTY, _0_0_EMPTY, _1_0_EMPTY, C));
    }

    @Test
    public void ins() {
        assertEquals(of(_0_0_EMPTY), SkewBinomialHeap.ins(_0_0_EMPTY, empty(), C));
        assertEquals(of(SkewBinomialHeap.link(_0_0_EMPTY, _0_0_EMPTY, C)),
                     SkewBinomialHeap.ins(_0_0_EMPTY, of(_0_0_EMPTY), C));

        assertEquals(of(_1_1_1, _0_0_EMPTY),
                     SkewBinomialHeap.ins(_0_0_EMPTY, of(_1_1_1), C));
    }

    @Test
    public void uniqify() {
        assertEquals(empty(), SkewBinomialHeap.uniqify(empty(), C));
        assertEquals(of(_0_0_EMPTY), SkewBinomialHeap.uniqify(of(_0_0_EMPTY), C));
        assertEquals(SkewBinomialHeap.ins(_0_0_EMPTY, of(_1_0_EMPTY), C),
                     SkewBinomialHeap.uniqify(of(_0_0_EMPTY, _1_0_EMPTY), C));
    }

    @Test
    public void meldUniq() {
        assertEquals(empty(), SkewBinomialHeap.meldUniq(empty(), empty(), C));
        assertEquals(of(_0_0_EMPTY), SkewBinomialHeap.meldUniq(of(_0_0_EMPTY), empty(), C));
        assertEquals(of(_1_0_EMPTY), SkewBinomialHeap.meldUniq(empty(), of(_1_0_EMPTY), C));

        assertEquals(SkewBinomialHeap.meldUniq(empty(), of(_0_1_0), C).cons(_1_0_EMPTY),
                     SkewBinomialHeap.meldUniq(of(_1_0_EMPTY), of(_0_1_0), C));

        assertEquals(SkewBinomialHeap.meldUniq(of(_0_1_0), empty(), C).cons(_1_0_EMPTY),
                     SkewBinomialHeap.meldUniq(of(_0_1_0), of(_1_0_EMPTY), C));


        assertEquals(SkewBinomialHeap.ins(SkewBinomialHeap.link(_0_0_EMPTY, _1_0_EMPTY, C),
                                          empty(), C),
                     SkewBinomialHeap.meldUniq(of(_0_0_EMPTY), of(_1_0_EMPTY), C));

        assertEquals(SkewBinomialHeap.ins(SkewBinomialHeap.link(_0_0_EMPTY, _1_0_EMPTY, C),
                                          of(_3_0_EMPTY), C),
                     SkewBinomialHeap.meldUniq(of(_0_0_EMPTY), of(_3_0_EMPTY, _1_0_EMPTY), C));

        assertEquals(SkewBinomialHeap.ins(SkewBinomialHeap.link(_0_0_EMPTY, _1_0_EMPTY, C),
                                          of(_2_0_EMPTY), C),
                     SkewBinomialHeap.meldUniq(of(_2_0_EMPTY, _0_0_EMPTY), of(_1_0_EMPTY), C));

        assertEquals(SkewBinomialHeap.ins(SkewBinomialHeap.link(_0_0_EMPTY, _1_0_EMPTY, C),
                                          SkewBinomialHeap.ins(SkewBinomialHeap.link(_2_0_EMPTY, _3_0_EMPTY, C),
                                                               empty(), C), C),
                     SkewBinomialHeap.meldUniq(of(_2_0_EMPTY, _0_0_EMPTY), of(_3_0_EMPTY, _1_0_EMPTY), C));

        assertEquals(SkewBinomialHeap.ins(
                SkewBinomialHeap.link(_0_0_EMPTY, _1_0_EMPTY, C),
                SkewBinomialHeap.ins(
                        SkewBinomialHeap.link(_2_0_EMPTY, _3_0_EMPTY, C),
                        SkewBinomialHeap.ins(
                                SkewBinomialHeap.link(_4_0_EMPTY, _5_0_EMPTY, C),
                                empty(), C), C), C),
                     SkewBinomialHeap.meldUniq(of(_4_0_EMPTY, _2_0_EMPTY, _0_0_EMPTY),
                                               of(_5_0_EMPTY, _3_0_EMPTY, _1_0_EMPTY), C));

        assertEquals(SkewBinomialHeap.ins(
                SkewBinomialHeap.link(_0_1_0, _0_1_0, C),
                SkewBinomialHeap.ins(
                        SkewBinomialHeap.link(_0_0_EMPTY, _0_0_EMPTY, C),
                        SkewBinomialHeap.meldUniq(of(_0_1_0), empty(), C),
                        C),
                C).cons(_0_0_EMPTY),
                     SkewBinomialHeap.meldUniq(of(_0_1_0, _0_0_EMPTY, _0_1_0),
                                               of(_0_0_EMPTY, _0_1_0, _0_0_EMPTY), C));

        assertEquals((SkewBinomialHeap.meldUniq(of(_0_1_0, _0_1_0, _0_1_0),
                                                empty(),
                                                C).cons(_0_0_EMPTY).cons(_0_0_EMPTY)).cons(_0_0_EMPTY),
                     SkewBinomialHeap.meldUniq(of(_0_0_EMPTY, _0_0_EMPTY, _0_0_EMPTY),
                                               of(_0_1_0, _0_1_0, _0_1_0), C));

        assertEquals((SkewBinomialHeap.meldUniq(of(_0_1_0, _0_1_0, _0_1_0),
                                                empty(),
                                                C).cons(_0_0_EMPTY).cons(_0_0_EMPTY)).cons(_0_0_EMPTY),
                     SkewBinomialHeap.meldUniq(of(_0_1_0, _0_1_0, _0_1_0),
                                               of(_0_0_EMPTY, _0_0_EMPTY, _0_0_EMPTY), C));
    }

    @Test
    public void insert() {
        assertEquals(of(_0_0_EMPTY), SkewBinomialHeap.insert(0, empty(), C));
        assertEquals(of(_1_0_EMPTY, _0_0_EMPTY), SkewBinomialHeap.insert(0, of(_1_0_EMPTY), C));
        assertEquals(of(_0_0_EMPTY, _1_0_EMPTY), SkewBinomialHeap.insert(1, of(_0_0_EMPTY), C));

        assertEquals(of(SkewBinomialHeap.skewLink(_0_0_EMPTY, _1_0_EMPTY, _2_0_EMPTY, C)),
                     SkewBinomialHeap.insert(0, of(_2_0_EMPTY, _1_0_EMPTY), C));

        assertEquals(of(_3_0_EMPTY, SkewBinomialHeap.skewLink(_0_0_EMPTY, _1_0_EMPTY, _2_0_EMPTY, C)),
                     SkewBinomialHeap.insert(0, of(_3_0_EMPTY, _2_0_EMPTY, _1_0_EMPTY), C));

        assertEquals(of(_2_1_2, _1_0_EMPTY, _0_0_EMPTY),
                     SkewBinomialHeap.insert(0, of(_2_1_2, _1_0_EMPTY), C));
        assertEquals(of(_1_0_EMPTY, _2_1_2, _0_0_EMPTY),
                     SkewBinomialHeap.insert(0, of(_1_0_EMPTY, _2_1_2), C));
    }

    @Test
    public void findMin() {
        assertEquals(nothing(), SkewBinomialHeap.findMin(empty(), C));
        assertEquals(just(0), SkewBinomialHeap.findMin(of(_0_0_EMPTY), C));
        assertEquals(just(0), SkewBinomialHeap.findMin(of(_0_0_EMPTY, _1_0_EMPTY), C));
        assertEquals(just(0), SkewBinomialHeap.findMin(of(_1_0_EMPTY, _0_0_EMPTY), C));
        assertEquals(just(0), SkewBinomialHeap.findMin(of(_0_0_EMPTY, _0_0_EMPTY), C));

        assertEquals(just(0), SkewBinomialHeap.findMin(of(_1_0_EMPTY, _0_1_0), C));
        assertEquals(just(0), SkewBinomialHeap.findMin(of(_0_1_0, _1_0_EMPTY), C));
        assertEquals(just(0), SkewBinomialHeap.findMin(of(_0_1_0, _1_1_1), C));

        assertEquals(just(1), SkewBinomialHeap.findMin(of(_1_1_1, _2_1_2), C));
        assertEquals(just(1), SkewBinomialHeap.findMin(of(_2_1_2, _1_1_1), C));
        assertEquals(just(0), SkewBinomialHeap.findMin(of(_2_1_2, _0_1_0), C));

        assertEquals(just(0),
                     SkewBinomialHeap.findMin(of(_1_0_EMPTY, _1_0_EMPTY, _1_0_EMPTY, _1_0_EMPTY, _0_0_EMPTY), C));
    }

    @Test
    public void getMin() {
        assertEquals(tuple(_0_0_EMPTY, empty()), SkewBinomialHeap.getMin(_0_0_EMPTY, empty(), C));
        assertEquals(tuple(_0_0_EMPTY, of(_0_0_EMPTY)), SkewBinomialHeap.getMin(_0_0_EMPTY, of(_0_0_EMPTY), C));

        assertEquals(tuple(_0_0_EMPTY, of(_1_0_EMPTY)), SkewBinomialHeap.getMin(_0_0_EMPTY, of(_1_0_EMPTY), C));
        assertEquals(tuple(_0_0_EMPTY, of(_1_0_EMPTY)), SkewBinomialHeap.getMin(_1_0_EMPTY, of(_0_0_EMPTY), C));

        assertEquals(tuple(_0_0_EMPTY, of(_2_0_EMPTY, _1_0_EMPTY)),
                     SkewBinomialHeap.getMin(_1_0_EMPTY, of(_0_0_EMPTY, _2_0_EMPTY), C));
        assertEquals(tuple(_0_0_EMPTY, of(_1_0_EMPTY, _2_0_EMPTY)),
                     SkewBinomialHeap.getMin(_0_0_EMPTY, of(_1_0_EMPTY, _2_0_EMPTY), C));
        assertEquals(tuple(_0_0_EMPTY, of(_1_0_EMPTY, _2_0_EMPTY)),
                     SkewBinomialHeap.getMin(_2_0_EMPTY, of(_0_0_EMPTY, _1_0_EMPTY), C));

        assertEquals(tuple(_0_0_EMPTY, of(_0_1_0, _2_0_EMPTY)),
                     SkewBinomialHeap.getMin(_0_0_EMPTY, of(_0_1_0, _2_0_EMPTY), C));
        assertEquals(tuple(_0_1_0, of(_0_0_EMPTY, _2_0_EMPTY)),
                     SkewBinomialHeap.getMin(_0_1_0, of(_0_0_EMPTY, _2_0_EMPTY), C));
    }

    @Test
    public void split() {
        assertEquals(tuple(StrictStack.<Node<Object>>empty(), empty()),
                     SkewBinomialHeap.split(empty(), empty(), empty()));

        assertEquals(tuple(of(_0_0_EMPTY), of(0)),
                     SkewBinomialHeap.split(of(_0_0_EMPTY), of(0), empty()));

        assertEquals(tuple(of(_0_0_EMPTY), of(0)),
                     SkewBinomialHeap.split(of(_0_0_EMPTY), empty(), of(_0_0_EMPTY)));

        assertEquals(tuple(of(_0_0_EMPTY), of(0, 0)),
                     SkewBinomialHeap.split(of(_0_0_EMPTY), of(0), of(_0_0_EMPTY)));

        assertEquals(tuple(of(_0_0_EMPTY), of(0, 1)),
                     SkewBinomialHeap.split(of(_0_0_EMPTY), of(0), of(_1_0_EMPTY)));

        assertEquals(tuple(of(_0_0_EMPTY, _0_1_0), of(0, 1)),
                     SkewBinomialHeap.split(of(_0_0_EMPTY), of(0), of(_0_1_0, _1_0_EMPTY)));
    }

    @Test
    public void deleteMin() {
        assertEquals(empty(), SkewBinomialHeap.deleteMin(empty(), C));
        assertEquals(empty(), SkewBinomialHeap.deleteMin(of(_0_0_EMPTY), C));

        assertEquals(of(_0_0_EMPTY), SkewBinomialHeap.deleteMin(of(_0_0_EMPTY, _0_0_EMPTY), C));
        assertEquals(of(_1_0_EMPTY), SkewBinomialHeap.deleteMin(of(_0_0_EMPTY, _1_0_EMPTY), C));
        assertEquals(of(_1_0_EMPTY), SkewBinomialHeap.deleteMin(of(_1_0_EMPTY, _0_0_EMPTY), C));

        assertEquals(of(new Node<>(1, 1, of(_2_0_EMPTY))),
                     SkewBinomialHeap.deleteMin(of(_2_0_EMPTY, _1_0_EMPTY, _0_0_EMPTY), C));
        assertEquals(of(new Node<>(1, 1, of(_2_0_EMPTY))),
                     SkewBinomialHeap.deleteMin(of(_1_0_EMPTY, _2_0_EMPTY, _0_0_EMPTY), C));

        assertEquals(of(new Node<>(1, 2, of(_1_0_EMPTY, _2_1_2))),
                     SkewBinomialHeap.deleteMin(of(_1_1_1, _2_1_2, _0_0_EMPTY), C));
        assertEquals(of(new Node<>(1, 2, of(_1_0_EMPTY, _2_1_2))),
                     SkewBinomialHeap.deleteMin(of(_2_1_2, _1_1_1, _0_0_EMPTY), C));

        assertEquals(of(new Node<>(1, 2, of(_1_0_EMPTY, _2_1_2)), _0_0_EMPTY),
                     SkewBinomialHeap.deleteMin(of(_2_1_2, _1_1_1, _0_1_0), C));
    }
}