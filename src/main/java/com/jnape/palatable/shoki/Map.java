package com.jnape.palatable.shoki;

import com.jnape.palatable.lambda.adt.hlist.Tuple2;

public interface Map<Size extends Number, K, V> extends Collection<Size, Tuple2<K, V>>, RandomAccess<K, V> {

    Map<Size, K, V> put(K k, V v);

    Map<Size, K, V> remove(K k);
}
