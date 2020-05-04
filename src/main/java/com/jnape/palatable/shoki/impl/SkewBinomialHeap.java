package com.jnape.palatable.shoki.impl;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.lambda.functions.Fn1;

import java.util.Comparator;
import java.util.Objects;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.Fn0.fn0;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Constantly.constantly;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Downcast.downcast;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into3.into3;
import static com.jnape.palatable.lambda.functions.builtin.fn2.LT.lt;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Map.map;
import static com.jnape.palatable.lambda.functions.builtin.fn2.ReduceLeft.reduceLeft;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.functions.builtin.fn3.LTEWith.lteWith;
import static com.jnape.palatable.lambda.functions.builtin.fn3.LTWith.ltWith;
import static com.jnape.palatable.lambda.functions.recursion.RecursiveResult.recurse;
import static com.jnape.palatable.lambda.functions.recursion.RecursiveResult.terminate;
import static com.jnape.palatable.lambda.functions.recursion.Trampoline.trampoline;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.equivalent;
import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.OrderedCollection.EquivalenceRelations.sameElementsSameOrder;
import static com.jnape.palatable.shoki.impl.StrictStack.empty;
import static com.jnape.palatable.shoki.impl.StrictStack.of;

final class SkewBinomialHeap {

    static <A> Node<A> link(Node<A> t1, Node<A> t2, Comparator<? super A> comp) {
        A x1 = t1.value;
        A x2 = t2.value;
        return lteWith(comp, x2, x1)
               ? new Node<>(x1, t1.rank + 1, t1.children.cons(t2))
               : new Node<>(x2, t2.rank + 1, t2.children.cons(t1));
    }

    static <A> Node<A> skewLink(Node<A> t0, Node<A> t1, Node<A> t2, Comparator<? super A> comp) {
        A x0 = t0.value;
        A x1 = t1.value;
        A x2 = t2.value;
        return lteWith(comp, x0, x1) && lteWith(comp, x2, x1)
               ? new Node<>(x1, t1.rank + 1, t1.children.cons(t2).cons(t0))
               : lteWith(comp, x0, x2) && lteWith(comp, x1, x2)
                 ? new Node<>(x2, t2.rank + 1, t2.children.cons(t1).cons(t0))
                 : new Node<>(x0, t0.rank + 1, of(t1, t2));
    }

    static <A> StrictStack<Node<A>> ins(Node<A> t, StrictStack<Node<A>> nodes, Comparator<? super A> comp) {
        return trampoline(
                into((t_, ts) -> ts.head().match(
                        fn0(() -> terminate(of(t_))),
                        t__ -> lt(t__.rank, t_.rank)
                               ? terminate(ts.cons(t_))
                               : recurse(tuple(link(t_, t__, comp), ts.tail())))),
                tuple(t, nodes));
    }

    static <A> StrictStack<Node<A>> uniqify(StrictStack<Node<A>> nodes,
                                            Comparator<? super A> comp) {
        return nodes.head().match(constantly(nodes), t -> ins(t, nodes.tail(), comp));
    }

    static <A> StrictStack<Node<A>> heapify(StrictStack<Node<A>> nodes, Comparator<? super A> comp) {
        return nodes.head().match(constantly(nodes), t -> ins(t, nodes.tail(), comp));
    }

    static <A> StrictStack<Node<A>> meldUniq(StrictStack<Node<A>> tss1, StrictStack<Node<A>> tss2,
                                             Comparator<? super A> comp) {
        return trampoline(into3((ts1, ts2, q) -> ts1.head().match(
                fn0(() -> terminate(foldLeft((r, f) -> f.apply(r), ts2, q))),
                t1 -> ts2.head().match(
                        fn0(() -> terminate(foldLeft((r, f) -> f.apply(r), ts1, q))),
                        t2 -> {
                            StrictStack<Node<A>> ts1_ = ts1.tail();
                            StrictStack<Node<A>> ts2_ = ts2.tail();
                            return lt(t2.rank, t1.rank)
                                   ? recurse(tuple(ts1_, ts2, q.cons(r -> r.cons(t1))))
                                   : lt(t1.rank, t2.rank)
                                     ? recurse(tuple(ts1, ts2_, q.cons(r -> r.cons(t2))))
                                     : recurse(tuple(ts1_, ts2_, q.cons(r -> ins(link(t1, t2, comp), r, comp))));
                        }))), tuple(tss1, tss2, StrictStack.<Fn1<StrictStack<Node<A>>, StrictStack<Node<A>>>>empty()));

    }

    static <A> StrictStack<Node<A>> insert(A a, StrictStack<Node<A>> ts, Comparator<? super A> comp) {
//        if (ts.isEmpty())
        return ts.head().flatMap(t1 -> ts.tail().head().fmap(t2 -> {
            StrictStack<Node<A>> rest = ts.tail().tail();
            return t1.rank == t2.rank
                   ? rest.cons(skewLink(new Node<>(a, 0, empty()), t1, t2, comp))
                   : ts.cons(new Node<>(a, 0, empty()));
        })).orElseGet(() -> ts.cons(new Node<>(a, 0, empty())));
    }

    static <A> StrictStack<Node<A>> meld(StrictStack<Node<A>> ts, StrictStack<Node<A>> ts_,
                                         Comparator<? super A> comp) {
        return meldUniq(uniqify(ts, comp), uniqify(ts_, comp), comp);
    }

    static <A> Maybe<A> findMin(StrictStack<Node<A>> nodes, Comparator<? super A> comp) {
        return reduceLeft((x, y) -> ltWith(comp, x, y) ? y : x, map(n -> n.value, nodes));
    }

    static <A> Tuple2<Node<A>, StrictStack<Node<A>>> getMin(Node<A> t, StrictStack<Node<A>> ts,
                                                            Comparator<? super A> comp) {
        return ts.head().match(fn0(() -> tuple(t, ts)),
                               t__ -> getMin(t__, ts.tail(), comp).into(
                                       (t_, ts_) -> lteWith(comp, t_.value, t.value)
                                                    ? tuple(t, ts)
                                                    : tuple(t_, ts_.cons(t))));
    }

    static <A> Tuple2<StrictStack<Node<A>>, StrictStack<A>> split(StrictStack<Node<A>> ts,
                                                                  StrictStack<A> xs,
                                                                  StrictStack<Node<A>> cs) {
        return trampoline(
                into3((ts_, xs_, cs_) -> cs_.head().match(
                        fn0(() -> terminate(tuple(ts_, xs_))),
                        t -> {
                            StrictStack<Node<A>> c = cs_.tail();
                            return t.rank == 0
                                   ? recurse(tuple(ts_, xs_.cons(t.value), c))
                                   : recurse(tuple(ts_.cons(t), xs_, c));
                        })),
                tuple(ts, xs, cs));
    }

    static <A> StrictStack<Node<A>> deleteMin(StrictStack<Node<A>> ts0, Comparator<? super A> comp) {
        return ts0.head().match(constantly(ts0),
                                t -> getMin(t, ts0.tail(), comp).into(
                                        (n, ts) -> split(empty(), empty(), n.children).into(
                                                (ts_, xs_) -> foldLeft((a, b) -> insert(b, a, comp),
                                                                       meld(ts, ts_, comp),
                                                                       xs_))));
    }

    static final class Node<A> {
        final A                    value;
        final int                  rank;
        final StrictStack<Node<A>> children;

        Node(A value, int rank, StrictStack<Node<A>> children) {
            this.value    = value;
            this.rank     = rank;
            this.children = children;
        }

        @Override
        public String toString() {
            return "Node{" +
                    "value=" + value +
                    ", rank=" + rank +
                    ", children=" + children +
                    '}';
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Node<A> node = downcast(o);
            return Objects.equals(value, node.value) &&
                    Objects.equals(rank, node.rank) &&
                    equivalent(children, node.children, sameElementsSameOrder(objectEquals()));
        }

        @Override
        public int hashCode() {
            return Objects.hash(value, rank, children);
        }
    }
}
