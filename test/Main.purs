module Test.Main where

import Prelude

import Data.Map as Map
import Data.Map.Functional as FMap
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Benchmark as B

type NMap = Map.Map String Int

type SMap = StrMap.StrMap Int

type FMap = FMap.Map String Int

assoclist :: Int -> Array (Tuple String Int)
assoclist l = xs where
  g n = Tuple (show n) n
  f n | n <= l = Just (Tuple (g n) (n + 1))
  f _ = Nothing
  xs = unfoldr f 0

nmap :: Int -> NMap
nmap n = Map.fromFoldable $ assoclist n

smap :: Int -> SMap
smap n = StrMap.fromFoldable $ assoclist n

fmap :: Int -> FMap
fmap n = FMap.fromFoldable $ assoclist n

nmapfind :: Int -> NMap -> Maybe Int
nmapfind i = Map.lookup (show i)

smapfind :: Int -> SMap -> Maybe Int
smapfind i = StrMap.lookup (show i)

fmapfind :: Int -> FMap -> Maybe Int
fmapfind i = FMap.lookup (show i)

kill :: forall a. a -> Maybe a
kill _ = Nothing

nmapupdate :: Int -> NMap -> NMap
nmapupdate n = Map.update kill (show n)

smapupdate :: Int -> SMap -> SMap
smapupdate n = StrMap.update kill (show n)

fmapupdate :: Int -> FMap -> FMap
fmapupdate n = FMap.update kill (show n)

nmapunion :: Int -> NMap
nmapunion n = Map.union (nmap n) (nmap n)

smapunion :: Int -> SMap
smapunion n = StrMap.union (smap n) (smap n)

fmapunion :: Int -> FMap
fmapunion n = FMap.union (fmap n) (fmap n)

main :: forall e. B.ReportEff e Unit
main = do
  B.report =<< B.benchmark "test" [ B.variant "nmap find 1000" (nmapfind 1000) (nmap 1000)
                                  , B.variant "nmap find 5000" (nmapfind 5000) (nmap 5000)
                                  , B.variant "nmap find 10000" (nmapfind 10000) (nmap 10000)
                                  , B.variant "smap find 1000" (smapfind 1000) (smap 1000)
                                  , B.variant "smap find 5000" (smapfind 5000) (smap 5000)
                                  , B.variant "smap find 10000" (smapfind 10000) (smap 10000)
                                  , B.variant "fmap find 1000" (fmapfind 1000) (fmap 1000)
                                  , B.variant "fmap find 5000" (fmapfind 5000) (fmap 5000)
                                  , B.variant "fmap find 10000" (fmapfind 10000) (fmap 10000)
                                  , B.variant "nmap update 1000" (nmapupdate 1000) (nmap 1000)
                                  , B.variant "nmap update 5000" (nmapupdate 5000) (nmap 5000)
                                  , B.variant "nmap update 10000" (nmapupdate 10000) (nmap 10000)
                                  , B.variant "smap update 1000" (smapupdate 1000) (smap 1000)
                                  , B.variant "smap update 5000" (smapupdate 5000) (smap 5000)
                                  , B.variant "smap update 10000" (smapupdate 10000) (smap 10000)
                                  , B.variant "fmap update 1000" (fmapupdate 1000) (fmap 1000)
                                  , B.variant "fmap update 5000" (fmapupdate 5000) (fmap 5000)
                                  , B.variant "fmap update 10000" (fmapupdate 10000) (fmap 10000)
                                  , B.variant "nmap union 1000" nmapunion 1000
                                  , B.variant "nmap union 5000" nmapunion 5000
                                  , B.variant "nmap union 10000" nmapunion 10000
                                  , B.variant "smap union 1000" smapunion 1000
                                  , B.variant "smap union 5000" smapunion 5000
                                  , B.variant "smap union 10000" smapunion 10000
                                  , B.variant "fmap union 1000" fmapunion 1000
                                  , B.variant "fmap union 5000" fmapunion 5000
                                  , B.variant "fmap union 10000" fmapunion 10000
                                  ]
