Shōki (正気)
======
[![Build Status](https://travis-ci.org/palatable/shoki.svg)](https://travis-ci.org/palatable/shoki)
[![Shōki](https://img.shields.io/maven-central/v/com.jnape.palatable/shoki.svg)](http://search.maven.org/#search%7Cga%7C1%7Ccom.jnape.palatable.shoki)

Purely functional, persistent data structures for the JVM.

#### Table of Contents

 - [Background](#background)
 - [Installation](#installation)
 - [Hierarchy](#hierarchy)
 - [Examples](#examples)
 - [Notes](#notes) 
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
logically exceed `Integer.MAX_VALUE`, invoking its `sizeInfo` should not require courage because of a risk of receiving
a potentially _negative value due to overflow_.

The target audience for _Shōki_ considers these characteristics not to be fine luxuries, but rather basic
quality-of-life essentials. If you think your time is too valuable to be spent staring at four methods with four
identical type signatures, trying to remember whether `peek` or `poll` throws or returns `null`, or `element` or
`remove` alters its underlying structure or doesn't; then you're in good company. Welcome!

<a name="installation">Installation</a>
------------

_Shōki_ is not yet released, but you can download its snapshots for alpha testing (make sure to have a Maven central
`repository` entry that enables `snapshots`).

Add the following dependency to your:

`pom.xml` ([Maven](https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html)):

```xml
<dependency>
    <groupId>com.jnape.palatable</groupId>
    <artifactId>shoki</artifactId>
    <version>1.0-SNAPSHOT</version>
</dependency>
```

`build.gradle` ([Gradle](https://docs.gradle.org/current/userguide/dependency_management.html)):

```gradle
compile group: 'com.jnape.palatable', name: 'shoki', version: '1.0-SNAPSHOT'
```

<a name="hierarchy">Hierarchy</a>
------------

One of the core design philophies of _Shōki_'s API is that its data structures are modeled as the incremental
composition of orthogonal capabilities and constraints. A `Collection` is a `Sequence` with a `Known` `SizeInfo`. A
`Set` is a `Collection` with `Membership` capability. A `Map<K, V>` is a `Collection` of `Tuple2<K, V>` with
`RandomAccess` from `K` to `Maybe<V>`.

The API type hierarchy can be visualized in the following diagram:



<a name="examples">Examples</a>
------------

<a name="notes">Notes</a>
-----

This is currently experimental, mostly to see how Java performs when the data structures are built on top of suspended, memoized computations.

<a name="license">License</a>
-------

_shōki_ is part of [palatable](http://www.github.com/palatable), which is distributed under [The MIT License](http://choosealicense.com/licenses/mit/).
