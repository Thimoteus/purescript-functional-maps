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

# installation
`bower i purescript-functional-maps`
