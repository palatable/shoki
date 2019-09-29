package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;

import java.util.Objects;

import static com.jnape.palatable.lambda.adt.hlist.HList.tuple;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Not.not;
import static com.jnape.palatable.lambda.functions.builtin.fn1.Tail.tail;
import static com.jnape.palatable.lambda.functions.builtin.fn2.CmpEq.cmpEq;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Cons.cons;
import static com.jnape.palatable.lambda.functions.builtin.fn2.DropWhile.dropWhile;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Eq.eq;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Find.find;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Into.into;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Iterate.iterate;
import static com.jnape.palatable.lambda.functions.builtin.fn2.LT.lt;
import static com.jnape.palatable.lambda.functions.builtin.fn2.Take.take;
import static com.jnape.palatable.lambda.functions.builtin.fn2.TakeWhile.takeWhile;
import static com.jnape.palatable.lambda.functions.builtin.fn3.FoldLeft.foldLeft;
import static com.jnape.palatable.lambda.monoid.builtin.Concat.concat;
import static com.jnape.palatable.shoki.ImmutableAvlTree.SearchAnswer.Found;
import static com.jnape.palatable.shoki.ImmutableAvlTree.SearchAnswer.Left;
import static com.jnape.palatable.shoki.ImmutableAvlTree.SearchAnswer.Right;

public final class ImmutableAvlTreeMap<K extends Comparable<K>, V> {

    private final ImmutableAvlTree<Bucket<K, V>> table;

    private ImmutableAvlTreeMap(ImmutableAvlTree<Bucket<K, V>> table) {
        this.table = table;
    }

    public ImmutableAvlTreeMap<K, V> put(K k, V v) {
        return new ImmutableAvlTreeMap<>(table.merge(Bucket.of(k, v), (oldB, newB) -> oldB.put(k, v)));
    }

    public Maybe<V> get(K k) {
        int searchKey = k.hashCode();
        return table.find(bucket -> cmpEq(bucket.hashCode, searchKey)
                                    ? Found
                                    : lt(bucket.hashCode, searchKey)
                                      ? Left
                                      : Right)
                .flatMap(b -> b.entryForKey(k));
    }

    public static <K extends Comparable<K>, V> ImmutableAvlTreeMap<K, V> empty() {
        return new ImmutableAvlTreeMap<>(ImmutableAvlTree.empty());
    }

    public static void main(String[] args) {

        Iterable<Integer>                    nats      = take(100_000, iterate(x -> x + 1, 0));
        ImmutableAvlTreeMap<String, Integer> denseTree = foldLeft((tree, x) -> tree.put("key " + x, x), empty(), nats);

        System.out.println(denseTree.put("key 8888", -1).get("key 8888"));

        System.out.println(denseTree.table.height());


    }

    private static final class Bucket<K extends Comparable<K>, V> implements Comparable<Bucket<K, V>> {
        private final int                          hashCode;
        private final ImmutableStack<Tuple2<K, V>> entries;

        private Bucket(int hashCode, ImmutableStack<Tuple2<K, V>> entries) {
            this.hashCode = hashCode;
            this.entries = entries;
        }

        @Override
        public int compareTo(Bucket<K, V> o) {
            return Integer.compare(hashCode, o.hashCode);
        }

        public Maybe<V> entryForKey(K searchKey) {
            return find(into((k, v) -> eq(searchKey, k)), entries).fmap(Tuple2::_2);
        }

        public Bucket<K, V> put(K k, V v) {
            Iterable<Tuple2<K, V>> front = takeWhile(not(eq(k).contraMap(Tuple2::_1)), entries);
            Iterable<Tuple2<K, V>> back  = tail(dropWhile(not(eq(k).contraMap(Tuple2::_1)), entries));
            return new Bucket<>(hashCode, ImmutableStack.of(concat(front, cons(tuple(k, v), back))));
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Bucket<?, ?> bucket = (Bucket<?, ?>) o;
            return hashCode == bucket.hashCode;
        }

        @Override
        public int hashCode() {
            return Objects.hash(hashCode);
        }

        public static <K extends Comparable<K>, V> Bucket<K, V> of(K k, V v) {
            return new Bucket<>(k.hashCode(), ImmutableStack.of(tuple(k, v)));
        }
    }
}
