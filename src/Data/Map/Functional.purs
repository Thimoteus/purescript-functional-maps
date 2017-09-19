module Data.Map.Functional where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

newtype Map k v = Map (k -> Maybe v)

derive instance newtypeMap :: Newtype (Map k v) _

instance functorMap :: Functor (Map k) where
  map :: forall a b. (a -> b) -> Map k a -> Map k b
  map f (Map a) = Map \ k -> Just f <*> a k

instance applyMap :: Apply (Map k) where
  apply :: forall a b. Map k (a -> b) -> Map k a -> Map k b
  apply (Map f) (Map a) = Map \ k -> f k <*> a k

-- | NOTE: `pure` will create a **total** function, unlike `singleton` which
-- | creates a function defined at only one key.
instance applicativeMap :: Applicative (Map k) where
  pure :: forall a. a -> Map k a
  pure v = Map \_ -> Just v

instance bindMap :: Bind (Map k) where
  bind :: forall a b. Map k a -> (a -> Map k b) -> Map k b
  bind (Map a) k = Map \ key -> do
    a' <- a key
    unwrap (k a') key

instance monadMap :: Monad (Map k)

instance semigroupMap :: Semigroup a => Semigroup (Map k a) where
  append :: Map k a -> Map k a -> Map k a
  append (Map a1) (Map a2) = Map \ k -> a1 k <> a2 k

instance monoidMap :: Semigroup a => Monoid (Map k a) where
  mempty = empty

instance semigroupoidMap :: Semigroupoid Map where
  compose (Map f) (Map g) = Map (f <=< g)

instance categoryMap :: Category Map where
  id = empty

fromTotal :: forall k v. (k -> v) -> Map k v
fromTotal f = Map \ str -> Just (f str)

foreign import fromPartialImpl
  :: forall a k v
   . a
  -> (Partial => k -> v)
  -> Maybe v
  -> (v -> Maybe v)
  -> k -> Maybe v

fromPartial :: forall k v. (Partial => k -> v) -> Map k v
fromPartial f = Map \ k -> fromPartialImpl unsafePartial f Nothing Just k

empty :: forall k v. Map k v
empty = Map \_ -> Nothing

singleton :: forall k v. Eq k => k -> v -> Map k v
singleton key val = insert key val empty

lookup :: forall k v. k -> Map k v -> Maybe v
lookup key (Map f) = f key

member :: forall k v. k -> Map k v -> Boolean
member key = isJust <<< lookup key

insert :: forall k v. Eq k => k -> v -> Map k v -> Map k v
insert key val (Map f) =
  Map \ k -> if k == key then Just val else f k

delete :: forall k v. Eq k => k -> Map k v -> Map k v
delete key (Map f) = Map \ k -> if k == key then Nothing else f k

pop :: forall k v. Eq k => k -> Map k v -> Maybe (Tuple v (Map k v))
pop key strmap = (\v -> Tuple v (delete key strmap)) <$> lookup key strmap

alter :: forall k v. Eq k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f key (Map m) = Map \ k -> if k == key then f (m key) else m k

update :: forall k v. Eq k => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = alter (maybe Nothing f) k m

fromFoldable :: forall f k v. Eq k => Foldable f => f (Tuple k v) -> Map k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty

fromFoldableWith :: forall f k v. Eq k => Foldable f => (v -> v -> v) -> f (Tuple k v) -> Map k v
fromFoldableWith f = foldl (\m (Tuple k v) -> alter (combine v) k m) empty where
  combine v (Just v') = Just $ f v v'
  combine v Nothing = Just v

-- | Left-biased union
union :: forall k v. Map k v -> Map k v -> Map k v
union (Map m1) (Map m2) = Map \ k -> maybe (m2 k) Just (m1 k)

unionWith :: forall k v. (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f (Map m1) (Map m2) = Map \ k ->
  let m1k = m1 k
      m2k = m2 k
   in case m1k of
      Just v -> case m2k of
        Just v' -> Just (f v v')
        _ -> m1k
      _ -> m2k

unions :: forall f k v. Foldable f => f (Map k v) -> Map k v
unions = foldl union empty

mapWithKey :: forall k v v'. (k -> v -> v') -> Map k v -> Map k v'
mapWithKey f (Map m) = Map \ k -> f k <$> m k

filter :: forall k v. (v -> Boolean) -> Map k v -> Map k v
filter f = filterWithKey $ const f

filterWithKey :: forall k v. (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey f (Map m) =
  Map \ k -> maybe Nothing (\v -> if f k v then Just v else Nothing) (m k)
