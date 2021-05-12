# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [Unreleased]

### Added
- `Shoki#hashMap` overload accepting `Iterable` of `java.util.Map.Entry`
- `Set#addAll`, a more permissive `Set#union` that accepts any shoki `Collection`

### Changed
- `Shoki`interop methods avoid unnecessary copying, where possible, via safe casts

## [1.0-alpha-2] - 2020-11-30
### Added
- `TreeMap`, `TreeSet`, and `TreeMultiSet`
- `Natural#times` and `Natural#modulo`

## [1.0-alpha-1] - 2020-05-11
### Added
- Initial API: `Collection`, `EquivalenceRelation`, `HashingAlgorithm`, `Map`, `Membership`, `MultiSet`, `Natural`, 
  `OrderedCollection`, `Queue`, `RandomAccess`, `Sequence`, `Set`, `Sizable`, `SizeInfo`, `Stack`
- Initial implementations: `HashMap`, `HashMultiSet`, `HashSet`, `StrictQueue`, `StrictStack`
- `Shoki`, a utility class exporting methods supporting interoperability with built-in JDK types

[Unreleased]: https://github.com/palatable/shoki/compare/shoki-1.0-alpha-2...HEAD
[1.0-alpha-2]: https://github.com/palatable/shoki/compare/shoki-1.0-alpha-1...shoki-1.0-alpha-2
[1.0-alpha-1]: https://github.com/palatable/shoki/commits/shoki-1.0-alpha-1


