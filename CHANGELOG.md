Changelog
=========

1.2.0.1
-------
- Remove the redundant `Monoid` instance for `Point`.

1.2.0
-----

#### Front End
- `VectorTile` now holds a lazy `Map` internally, instead of a `Vector`. Use
the `ix` Lens, giving it a layer name, to hone in on individual Layers
quickly.
- Users an optionally use the new `fromProtobuf` function directly from
`Geography.VectorTile.Protobuf` if they wish to be semantically explicit
about the backend conversion. Otherwise, they can still use the top-level
`tile` function.

#### Back End
- Reworked internals for Protobuf conversion. Most code moved to an
`Internal` module.

1.1.1
-----
- Removed the `StrictData` pragma. Turns out laziness is faster.
