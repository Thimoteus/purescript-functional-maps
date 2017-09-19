# purescript-functional-maps

A map is idiomatically a (usually finite) pairing of keys with values.
Set-theoretically, a map has the same representation as a function -- where a
function is just a relation `R ⊆ A × B` such that there are no two pairs `(a, b)
(a, c) ∈ R` with `b ≠ c`.

Thus, the map `{("zero", 0), ("one", 1)}` is the same as a (total) function
`f : {"zero", "one"} -> {0, 1}` such that `f : "zero" ↦ 0, "one" ↦ 1`.

Therefore, it is possible to express a map as an actual function:

```purescript
type Map k v = k -> Maybe v
```

where the `Maybe` is used to denote a function with a finite domain.

In the case of `f` above, however, the domain is `String`, so we must treat it as
a partial function:

```purescript
f :: Map String Int
f "zero" = Just 0
f "one" = Just 1
f _ = Nothing
```

# Usage

Many of the same functions in `Data.Map` and `Data.StrMap` have counterparts here,
though some do not.
When they do, the typeclass constraints are often relaxed or removed entirely:
for example,

```purescript
Data.Map.lookup :: ∀ k v. Ord k => k -> Map k v -> Maybe v
Data.Map.Functional.lookup :: ∀ k v. k -> Map k v -> Maybe v

Data.Map.insert :: ∀ k v. Ord k => k -> v -> Map k v -> Map k v
Data.Map.Functional.insert :: ∀ k v. Eq k => k -> v -> Map k v -> Map k v
```

Besides those differences, the usage is mostly the same.


There are a few ways of creating a `Map`: either by chaining `insert` on top of
an `empty`, using `fromFoldable` / `fromFoldableWith`, using the type constructor
directly (`mymap = Map f where f ...`) or using the following two:

```purescript
fromTotal :: ∀ k v. (k -> v) -> Map k v
fromPartial :: ∀ k v. (Partial => k -> v) -> Map k v
```

For example:

```purescript
finiteMap :: Map Int String
finiteMap = fromPartial f where
  f :: Partial => Int -> String
  f 0 = "zero"
  f 1 = "one"
```

# Performance

Benchmarking code is under the `test/` folder in the `benchmark` branch.
"nmap" refers to `Data.Map.Map String Int`; "smap" to `Data.StrMap.StrMap Int`; "fmap" to `Data.Map.Functional.Map String Int`.
In short:

```
* Build successful.
* Running tests...
Running benchmark test
          warmup            0.996 ns            0.999 r2   1003726370.335 ops/s
  nmap find 1000         1266.543 ns            0.999 r2       789550.595 ops/s
  nmap find 5000         1802.797 ns            0.999 r2       554693.597 ops/s
 nmap find 10000         1746.164 ns            1.000 r2       572683.835 ops/s
  smap find 1000           64.336 ns            1.000 r2     15543406.510 ops/s
  smap find 5000           63.776 ns            1.000 r2     15679907.156 ops/s
 smap find 10000           65.962 ns            1.000 r2     15160269.708 ops/s
  fmap find 1000           49.162 ns            1.000 r2     20340985.185 ops/s
  fmap find 5000           56.922 ns            1.000 r2     17567840.653 ops/s
 fmap find 10000           57.033 ns            1.000 r2     17533582.505 ops/s
nmap update 1000         6183.264 ns            1.000 r2       161726.883 ops/s
nmap update 5000         8240.696 ns            1.000 r2       121348.979 ops/s
nmap update 10000         7583.315 ns            0.999 r2       131868.443 ops/s
smap update 1000       272343.977 ns            1.000 r2         3671.827 ops/s
smap update 5000      1352224.081 ns            0.999 r2          739.522 ops/s
smap update 10000      2663867.041 ns            0.999 r2          375.394 ops/s
fmap update 1000          284.836 ns            1.000 r2      3510796.603 ops/s
fmap update 5000          285.392 ns            1.000 r2      3503950.768 ops/s
fmap update 10000          287.451 ns            1.000 r2      3478859.238 ops/s
 nmap union 1000     15244907.916 ns            0.999 r2           65.596 ops/s
 nmap union 5000     93190375.612 ns            0.997 r2           10.731 ops/s
nmap union 10000    195670113.429 ns            0.996 r2            5.111 ops/s
 smap union 1000      3681219.224 ns            0.999 r2          271.649 ops/s
 smap union 5000     18868260.511 ns            0.999 r2           52.999 ops/s
smap union 10000     41278161.276 ns            0.998 r2           24.226 ops/s
 fmap union 1000      1268219.466 ns            0.999 r2          788.507 ops/s
 fmap union 5000      7960383.056 ns            0.999 r2          125.622 ops/s
fmap union 10000     39351788.240 ns            0.932 r2           25.412 ops/s
* Tests OK.
```

Functional maps are on-par with or better than `StrMap`s under this benchmark, and much better than normal `Map`s.

But like all benchmarks, take it with a grain of salt -- if you *need* to know how performance of each holds up, write your own benchmarks tailored to your own use cases.

# installation
`bower i purescript-functional-maps`
