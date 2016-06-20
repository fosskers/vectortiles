VectorTiles
===========

What are VectorTiles?
---------------------
Invented by [Mapbox](https://www.mapbox.com/), they are a combination of the
ideas of finite-sized tiles and vector geometries. Mapbox maintains the
official implementation spec for VectorTile codecs.

VectorTiles are advantageous over raster tiles in that:

1. They are typically smaller to store
2. They can be easily transformed (rotated, etc.) in real time
3. They allow for continuous (as opposed to step-wise) zoom in Slippy Maps.

Raw VectorTile data is stored in the protobuf format. Any codec implementing
[the spec](https://github.com/mapbox/vector-tile-spec/tree/master/2.1) must
decode and encode data according to [this *.proto*
schema](https://github.com/mapbox/vector-tile-spec/blob/master/2.1/vector_tile.proto).

What is this library?
---------------------
`vectortiles` is a minimum viable implementation of **Version 2.1** of the
VectorTile spec. It aims to be a solid reference from which to implement
other codecs. It exposes a small API of conversion functions between raw
protobuf data and a higher-level `VectorTile` type that is more condusive to
further processing. It also exposes fairly simplistic (yet sensible)
implementations of the typical GIS `Geometry` types:

* Point
* LineString
* Polygon

For ease of encoding and decoding, each `Geometry` type and its `Multi`
counterpart (i.e. *Multipoint*) are considered the same thing, a `Vector` of
that `Geometry`.

#### Efficiency

This library is not micro-optimized, but does leverage some "for-free"
aspects of Haskell to remain usable:

* `Point` is implemented as a [Record Pattern
Synonym](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#record-patsyn)
to hide the fact it's just a vanilla tuple of `Int`s. This allows us to use
the more efficient unboxed `Vector`s with it:

```haskell
-- | Access a Point's values with the `x` and `y` functions.
type Point = (Int,Int)
pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)
```

* Some types (like `LineString`) are implemented as a `newtype` for its
compile-time unboxing:

```haskell
import qualified Data.Vector.Unboxed as U

newtype LineString = LineString { lsPoints :: U.Vector Point }
```

* We use the new `StrictData` language pragma from GHC8.
* All lenses are `INLINE`d.

#### Performance

You can run the benchmarks with `stack bench`, provided you have [the stack
tool](http://docs.haskellstack.org/en/stable/README/). The following results
are from a 2016 Lenovo ThinkPad Carbon X1 with an Intel Core i7 processor,
comparing this library with a [Python library of similar
functionality](https://github.com/mapzen/mapbox-vector-tile).

##### Decoding

| | One Point | One LineString | roads.mvt (40kb, 15 layers)
| --- | --- | --- | --- |
| CPython 3.5.1 | 60 μs | 70 μs | 73 ms |
| PyPy 5.3 | 115 μs | 210 μs | 11.6 ms |
| Haskell | 3.6 μs | 4.3 μs | 14.2 ms

##### Encoding

| | One Point | One LineString | roads.mvt (40kb, 15 layers)
| --- | --- | --- | --- |
| CPython 3.5.1 | 175 μs | 227 μs | N/A |
| Haskell | 3.1 μs | 3.9 μs | 10 ms

*Certain encoding benchmarks for Python were not possible.*

Questions & Issues
------------------

Simply parsing raw protobuf data is not enough to work with VectorTiles,
since the spec also defines how said data is to be interpreted once parsed.
In writing a codec, there are a number of things one must consider:

#### Hand written schema code vs `protoc` use

Many languages have a "protobuf compiler" which can take a `.proto` file and
generate schema code to access parsed data. There are PROs and CONs to taking
this approach.

PROs for using a `protoc`-like program:

* All accessor code is written for you
* Update process when new official `.proto` is released is clearer

PROs for writing your own schema:

* Its your code, so you have more control. Bugs are easier to chase
* The code will likely be much shorter

In the case of two Haskell protobuf libraries which were compared, the
hand-written one allowed for a 50-line schema, while the other
auto-generated a 550-line one.

#### Extension Support

The protobuf spec leaves room for additional:

* `Value` types in the key-value metadata maps
* fields in a `Layer`
* fields in a `Tile`
* use of the `UNKNOWN` geometry type

In writing a codec, you are completely free to ignore these. However, they
are permitted by the spec, and some tools may encode data using them.

#### Feature/Geometry Polymorphism

At the protobuf level, Features of Points, LineStrings, and Polygons are all mixed
into a single list, distinguished only by a `GeomType` label. At a high level, you may
wish to separate these specifically. This library does just that:

```haskell
data Layer = Layer { _version :: Int
                   , _name :: Text
                   , _points :: V.Vector (Feature Point)
                   , _linestrings :: V.Vector (Feature LineString)
                   , _polygons :: V.Vector (Feature Polygon)
                   , _extent :: Int
                   }
```
As opposed to having a single field named `features`, which contains all
features unified by some superclass / trait / generic.

*Claim:* Having separate accessors for each geometry type yields a "heavier"
API, but gives more power, is more performant, and less complex.

#### Layer / Feature Coupling

Layers and Features have coupled data at the protobuf level. In order to achieve
higher compression ratios, Layers contain all metadata in key/value lists to
be shared across their Features, while those Features store only indices
into those lists. As a result, functions converting protobuf-level Feature
objects into a high-level type need to be passed those key/value lists from
the parent Layer.  and a more isomorphic:

```haskell
feature :: Geometry g => RawFeature -> Either Text (Feature g)
```
is not possible.

#### Polygon Definition

Version 2 of the spec mainly clarified language surrounding how polygons
should be decoded. [This Github
issue](https://github.com/mapbox/vector-tile-spec/issues/80) reports another
"gotcha" associated with the definition of polygons.

#### Sanity Checks / Error Handling

The protobuf data can be malformed in a number of ways. How much sanity
checking you wish to do while decoding depends on how much performance you
can sacrifice. For instance, here is a constraint found in the spec
regarding feature metadata:

> Every key index MUST be unique within that feature such that no other
> attribute pair within that feature has the same key index. A feature MUST
> have an even number of tag fields. A feature tag field MUST NOT contain a
> key index or value index greater than or equal to the number of elements in
> the layer's keys or values set, respectively.

Tips
----
> Decode what you encode, and encode what you decode.

Your encoding and decoding functions should be as close to isomorphisms as possible.

> (0,0) is in the top-left corner.

> Know your binary arithmetic.

Lists of Geometry commands/values are Z-encoded. See the `zig` and `unzig`
functions in `Geometry.VectorTile.Geometry`.
