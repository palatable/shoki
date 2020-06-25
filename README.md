Shōki (正気)
======
[![Build Status](https://travis-ci.org/palatable/shoki.svg)](https://travis-ci.org/palatable/shoki)
[![Shōki](https://img.shields.io/maven-central/v/com.jnape.palatable/shoki.svg)](http://search.maven.org/#search%7Cga%7C1%7Ccom.jnape.palatable.shoki)
[![Shōki](https://img.shields.io/maven-metadata/v?color=aaa&label=development&metadataUrl=https%3A%2F%2Foss.sonatype.org%2Fcontent%2Frepositories%2Fsnapshots%2Fcom%2Fjnape%2Fpalatable%2Fshoki%2Fmaven-metadata.xml)](https://oss.sonatype.org/content/repositories/snapshots/com/jnape/palatable/shoki/)

Purely functional, persistent data structures for the JVM.

#### Table of Contents

 - [Background](#background)
 - [Installation](#installation)
 - [Hierarchy](#hierarchy)
 - [Implementations](#implementations)
 - [License](#license)

<a name="background">Background</a>
----------

_Shōki_ is a library offering purely functional, persistent data structures for the JVM. 

So why another data structures library? Because writing correct code is hard, and it's especially hard if your data
structures consistently advertise false interfaces. The data structures provided by _Shōki_ are built on top of
interfaces that promote type-checker participation by leveraging rich types, and encourage correct-by-construction
software with obvious usage semantics.

Standard usage of Shōki interfaces should never throw: there are no `IndexOutOfBoundsException`s that result from
seemingly innocuous `get` calls; no `UnsupportedOperationException`s because the interface you're presented with is an
_absolute fiction_, and somewhere under the covers is an immutable collection masquerading as a mutable one; no
`ClassCastExceptions` because a data structure that fundamentally only works in the presence of comparability fails 
to enforce this essential constraint at compile time. Any of these things happening is considered an error in _Shōki_,
not in user code.

The constraints of the inputs and the outputs are also richly specified via their type signatures. If a lookup cannot
be guaranteed to yield a value, it will return a
[`Maybe`](https://github.com/palatable/lambda/blob/master/src/main/java/com/jnape/palatable/lambda/adt/Maybe.java)
rather than `null` so the type-checker can remind you that you may not have received what you wanted. If a value
fundamentally cannot be zero, it will very likely be represented by a type that does not even include a "zero" term so
you don't waste mental energy writing code to handle impossible scenarios. If the total size of a collection can
logically exceed `Integer.MAX_VALUE`, its `sizeInfo` will advertise a type that can realistically represent its size
instead of one that might represent a negative value due to overflow.

The target audience for _Shōki_ considers these characteristics not to be fine luxuries, but rather basic
quality-of-life essentials. If you think your time is too valuable to be spent staring at four methods with four
identical type signatures, trying to remember whether `peek` or `poll` throws or returns `null`, or `element` or
`remove` alters its underlying structure or doesn't; then you're in good company. Welcome!

<a name="installation">Installation</a>
------------

_Shōki_ alpha releases are currently available in Maven Central. These alpha releases are meant to be of high enough
quality as to be reliable in a production environment, while still allowing significant API changes to occur before a
`1.0` release. 

Add the following dependency to your:

`pom.xml` ([Maven](https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html)):

```xml
<dependency>
    <groupId>com.jnape.palatable</groupId>
    <artifactId>shoki</artifactId>
    <version>1.0-alpha-1</version>
</dependency>
```

`build.gradle` ([Gradle](https://docs.gradle.org/current/userguide/dependency_management.html)):

```gradle
compile group: 'com.jnape.palatable', name: 'shoki', version: '1.0-alpha-1'
```

<a name="hierarchy">Hierarchy</a>
------------

One of the core design tenants of _Shōki_'s API is that a data structure is modeled as the incremental composition of
its orthogonal capabilities and constraints.

#### Top-level orthogonal building blocks

- `Sequence<A>`: an `Iterable<A>` interface that offers the methods `Maybe<A> head()` and `Sequence<A> tail()`
- `SizeInfo`: a coproduct of `Unknown` or `Known<N extends Number>`, where `Known<N>` offers a method `N getSize()`
- `Sizable`: an interface that offers a method `SizeInfo sizeInfo()`
- `Membership<A>`: an interface that offers a membership test method `boolean contains(A)`     
- `RandomAccess<Index, V>`: `Membership<Index>` with an additional lookup method `V get(Index)`     
- `Natural`: a `Number` sum type of `Zero` or `NonZero` that never overflows and offers basic type-safe arithmetic       

#### Refined data structure interfaces

- `Collection<Size extends Number, A>`: a `Sequence<A>` that supports `SizeInfo` yielding a `Known<Size>`
- `OrderedCollection<Size extends Number, A>`: a `Collection<Size, A>` that offers a `reverse()` method
- `Stack<Size extends Number, A>`: an `OrderedCollection<Size, A>` with a method `Stack<Size, A> cons(A)` that adds an
  element to the top of the `Stack<Size, A>`  
- `Queue<Size extends Number, A>`: an `OrderedCollection<Size, A>` with a method `Queue<Size, A> snoc(A)` that adds an
  element to the bottom of the `Queue<Size, A>`   
- `Set<Size extends Number, A>`: a `Collection<Size, A>` that supports `Membership<A>`
- `MultiSet<A>`: a `Collection<Natural, A>` of `Tuple2<A, Natural.NonZero>` that supports `RandomAccess<A, Natural>`
- `Map<Size extends Number, K, V>`: a `Collection<Size, Tuple2<K, V>>` that supports `RandomAccess<K, Maybe<V>>`

_Shōki_ is still very much in alpha development, so these specific interfaces are subject to change, but they should at
least it offer an intuition about the way in which the design is being approached. 

<a name="examples">Implementations</a>
------------

#### `StrictStack<A>`

A `StrictStack<A>` is a strictly-evaluated `Stack<Natural, A>` that offers worst-case `O(1)` space/time for `cons`,
`head`, and `tail`.  

```java
import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.impl.StrictStack;

import static com.jnape.palatable.shoki.impl.StrictStack.strictStack;

public class Example {

    public static void main(String[] args) {
        StrictStack<String> empty     = strictStack();
        boolean             _true     = empty.isEmpty();
        Maybe<String>       nothing   = empty.head();
        StrictStack<String> alsoEmpty = empty.tail();

        StrictStack<String> fooBarBaz = empty.cons("baz").cons("bar").cons("foo");
        boolean             _false    = fooBarBaz.isEmpty();
        Maybe<String>       justFoo   = fooBarBaz.head();

        StrictStack<String> barBaz  = fooBarBaz.tail();
        Maybe<String>       justBar = barBaz.head();

        StrictStack<String> baz     = barBaz.tail();
        Maybe<String>       justBaz = baz.head();

        StrictStack<String> bazBarFooBaz = baz.consAll(fooBarBaz);
        boolean             __true       = bazBarFooBaz.equals(strictStack("baz", "bar", "foo", "baz"));
    }
}
```

#### `StrictQueue<A>`

A `StrictQueue<A>` is a strictly-evaluated `Queue<Natural, A>` and `Stack<Natural, A>` that offers worst-case `O(1)`
space/time for `cons`, `snoc`, and `head`, and amortized `O(1)` for `tail`.

```java
import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.impl.StrictQueue;

import static com.jnape.palatable.shoki.impl.StrictQueue.strictQueue;

public class Example {

    public static void main(String[] args) {
        StrictQueue<String> empty     = strictQueue();
        boolean             _true     = empty.isEmpty();
        Maybe<String>       nothing   = empty.head();
        StrictQueue<String> alsoEmpty = empty.tail();

        StrictQueue<String> fooBarBaz     = empty.snoc("foo").snoc("bar").snoc("baz");
        boolean             _false        = fooBarBaz.isEmpty();
        Maybe<String>       justFoo       = fooBarBaz.head();
        StrictQueue<String> alsoFooBarBaz = empty.cons("baz").cons("bar").cons("foo");

        StrictQueue<String> barBaz  = fooBarBaz.tail();
        Maybe<String>       justBar = barBaz.head();

        StrictQueue<String> baz     = barBaz.tail();
        Maybe<String>       justBaz = baz.head();

        StrictQueue<String> bazFooBarBaz = baz.snocAll(fooBarBaz);
        StrictQueue<String> bazBarFooBaz = baz.consAll(fooBarBaz);
        boolean             __true       = bazFooBarBaz.equals(strictQueue("baz", "foo", "bar", "baz"));
    }
}
```

#### `HashMap<K, V>`

A `HashMap<K, V>` is an [ideal hash tree](https://lampwww.epfl.ch/papers/idealhashtrees.pdf) implementation of a
`Map<Natural, K, V>` that offers amortized `O(1)` space/time for `get`, `put`, `remove`, and `contains`, and supports
custom `EquivalenceRelation<K>`s and `HashingAlgorithm<K>`s.   

```java
import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.impl.HashMap;
import com.jnape.palatable.shoki.impl.HashSet;
import com.jnape.palatable.shoki.impl.StrictQueue;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.impl.HashMap.hashMap;

public class Example {

    public static void main(String[] args) {
        // same as hashMap()
        HashMap<Integer, String> empty = hashMap(objectEquals(), objectHashCode());

        boolean                        _true     = empty.isEmpty();
        Maybe<Tuple2<Integer, String>> nothing   = empty.head();
        HashMap<Integer, String>       alsoEmpty = empty.tail();

        HashMap<Integer, String>       fooBarBaz     = empty.put(0, "foo").put(1, "bar").put(2, "baz");
        boolean                        _false        = fooBarBaz.isEmpty();
        Maybe<Tuple2<Integer, String>> just0Foo      = fooBarBaz.head();
        HashMap<Integer, String>       alsoFooBarBaz = empty.put(2, "baz").put(1, "bar").put(0, "foo");
        boolean                        __true        = fooBarBaz.contains(0);
        boolean                        __false       = fooBarBaz.contains(-1);

        // This HashSet uses the same EquivalenceRelation and HashingAlgorithm
        HashSet<Integer> keys = fooBarBaz.keys(); // HashSet[0, 1, 2]

        StrictQueue<String> values = fooBarBaz.values(); // StrictQueue["foo", "bar", "baz"]

        HashMap<Integer, String>       barBaz   = fooBarBaz.tail();
        Maybe<Tuple2<Integer, String>> just1Bar = barBaz.head();

        HashMap<Integer, String>       baz      = barBaz.tail();
        Maybe<Tuple2<Integer, String>> just2Baz = baz.head();

        // HashMap[(2=bazbaz)]
        HashMap<Integer, String> _2bazbaz = baz.merge(baz, (s1, s2) -> s1 + s2);
    }
}
```

#### `HashSet<A>`

A `HashSet<A>` is a `Set<Natural, A>` that is backed by a `HashMap<A, Unit>` and offers similar space/time
complexities. Like `HashMap`, a `HashSet` supports custom `EquivalenceRelation<K>`s and `HashingAlgorithm<K>`s.

```java
import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.shoki.impl.HashSet;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.impl.HashSet.hashSet;

public class Example {

    public static void main(String[] args) {
        // same as hashSet()
        HashSet<Integer> empty = hashSet(objectEquals(), objectHashCode());

        boolean          _true     = empty.isEmpty();
        Maybe<Integer>   nothing   = empty.head();
        HashSet<Integer> alsoEmpty = empty.tail();

        HashSet<Integer> _012    = empty.add(0).add(1).add(2);
        boolean          _false  = _012.isEmpty();
        Maybe<Integer>   just0   = _012.head();
        HashSet<Integer> also012 = empty.add(2).add(1).add(0);
        boolean          __true  = _012.contains(0);
        boolean          __false = _012.contains(-1);


        HashSet<Integer> _12   = _012.tail();
        Maybe<Integer>   just1 = _12.head();

        HashSet<Integer> _2    = _12.tail();
        Maybe<Integer>   just2 = _2.head();

        HashSet<Integer> _01234 = _012.union(hashSet(2, 3, 4));
        HashSet<Integer> _01    = _012.difference(hashSet(2, 3, 4));
        HashSet<Integer> _0134  = _012.symmetricDifference(hashSet(2, 3, 4));
        HashSet<Integer> __2    = _012.intersection(hashSet(2, 3, 4));
    }
}
```

#### `HashMultiSet<A>`

A `HashMultiSet<A>` is a `MultiSet<A>` that is backed by a `HashMap<A, Natural.NonZero>` and offers similar space/time
complexities. Like `HashMap`, a `HashMultiSet` supports custom `EquivalenceRelation<K>`s and `HashingAlgorithm<K>`s.

```java
import com.jnape.palatable.lambda.adt.Maybe;
import com.jnape.palatable.lambda.adt.hlist.Tuple2;
import com.jnape.palatable.shoki.api.Natural;
import com.jnape.palatable.shoki.api.Natural.NonZero;
import com.jnape.palatable.shoki.impl.HashMultiSet;
import com.jnape.palatable.shoki.impl.HashSet;

import static com.jnape.palatable.shoki.api.EquivalenceRelation.objectEquals;
import static com.jnape.palatable.shoki.api.HashingAlgorithm.objectHashCode;
import static com.jnape.palatable.shoki.impl.HashMultiSet.hashMultiSet;

public class Example {

    public static void main(String[] args) {
        // same as hashMultiSet()
        HashMultiSet<Integer> empty = hashMultiSet(objectEquals(), objectHashCode());

        boolean                         _true     = empty.isEmpty();
        Maybe<Tuple2<Integer, NonZero>> nothing   = empty.head();
        HashMultiSet<Integer>           alsoEmpty = empty.tail();

        HashMultiSet<Integer>           _0x1_1x2_2x1     = empty.inc(0).inc(1).inc(2).inc(1);
        boolean                         _false           = _0x1_1x2_2x1.isEmpty();
        Maybe<Tuple2<Integer, NonZero>> just0x1          = _0x1_1x2_2x1.head();
        HashMultiSet<Integer>           also_0x1_1x2_2x1 = empty.inc(1).inc(2).inc(1).inc(0);
        boolean                         __true           = _0x1_1x2_2x1.contains(0);
        boolean                         __false          = _0x1_1x2_2x1.contains(-1);

        Natural one = _0x1_1x2_2x1.get(0);
        Natural two = _0x1_1x2_2x1.get(1);

        HashMultiSet<Integer>           _1x2_2x1 = _0x1_1x2_2x1.tail();
        Maybe<Tuple2<Integer, NonZero>> just_1x2 = _1x2_2x1.head();

        HashMultiSet<Integer>           _2x1     = _1x2_2x1.tail();
        Maybe<Tuple2<Integer, NonZero>> just_2x1 = _2x1.head();

        HashMultiSet<Integer> _2x1_3x1_4x1         = hashMultiSet(2, 3, 4);
        HashMultiSet<Integer> _0x1_1x2_2x1_3x1_4x1 = _0x1_1x2_2x1.union(_2x1_3x1_4x1);
        HashMultiSet<Integer> _0x1_1x2             = _0x1_1x2_2x1.difference(_2x1_3x1_4x1);
        HashMultiSet<Integer> _0x1_1x2_3x1_4x1     = _0x1_1x2_2x1.symmetricDifference(_2x1_3x1_4x1);
        HashMultiSet<Integer> __2x1                = _0x1_1x2_2x1.intersection(_2x1_3x1_4x1);
        HashMultiSet<Integer> _0x1_1x2_2x2_3x1_4x1 = _0x1_1x2_2x1.sum(_2x1_3x1_4x1);

        HashSet<Integer> _012 = _0x1_1x2_2x1.unique();
    }
}
```

<a name="license">License</a>
-------

_shōki_ is part of [palatable](http://www.github.com/palatable), which is distributed under 
[The MIT License](http://choosealicense.com/licenses/mit/).
