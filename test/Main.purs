module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Data.Map.Functional as Map
import Data.Maybe (Maybe(..))
import Data.StrMap.Functional (StrMap)
import Data.Tuple (Tuple(..), fst, snd)

type Test a = Eff (console :: CONSOLE) a

strMap1 :: StrMap Int
strMap1 = Map.fromPartial f
  where
  f :: Partial => String -> Int
  f "hello" = 5

strMap2 :: StrMap String
strMap2 = Map.fromTotal f
  where
  f "hello" = "hello"
  f _ = ""

strMap3 :: StrMap Boolean
strMap3 = Map.empty # Map.insert "one" true # Map.insert "two" false

partialTest :: Test Unit
partialTest = do
  log "Should print (Just 5)"
  logShow $ Map.lookup "hello" strMap1
  log "Should print Nothing"
  logShow $ Map.lookup "hello again" strMap1

totalTest :: Test Unit
totalTest = do
  log "Should print (Just \"hello\")"
  logShow $ Map.lookup "hello" strMap2
  log "Should print (Just \"\")"
  logShow $ Map.lookup "goodbye" strMap2

insertTest :: Test Unit
insertTest = do
  log "Should print Just values for true and false"
  logShow $ Map.lookup "one" strMap3
  logShow $ Map.lookup "two" strMap3
  log "Should print Nothing"
  logShow $ Map.lookup "three" strMap3

deleteTest :: Test Unit
deleteTest = do
  log "Should print Nothing"
  logShow $ Map.lookup "one" $ Map.delete "one" strMap3

popTest :: Test Unit
popTest = do
  let map' = Map.pop "one" strMap3
      justOne = fst <$> map'
      justMap = snd <$> map'
      nothing = join $ Map.lookup "one" <$> justMap
  log "Should print (Just true)"
  logShow justOne
  log "Should print Nothing"
  logShow nothing

alterTest :: Test Unit
alterTest = do
  let alter1 (Just 1) = Just 0
      alter1 (Just 2) = Nothing
      alter1 _ = Just (-1)
      mymap =
        Map.singleton "zero" 0 # Map.insert "one" 1 # Map.insert "two" 2
      mymap'
        = mymap
        # Map.alter alter1 "zero" -- blanket modify
        # Map.alter alter1 "one" -- modify
        # Map.alter alter1 "two" -- delete
        # Map.alter alter1 "threeve" -- insert
  log "Should print (Just 0)"
  logShow $ Map.lookup "one" mymap'
  log "Should print Nothing"
  logShow $ Map.lookup "two" mymap'
  log "Should print (Just (-1))"
  logShow $ Map.lookup "zero" mymap'
  log "Should print (Just (-1))"
  logShow $ Map.lookup "threeve" mymap'

fromFoldableTest :: Test Unit
fromFoldableTest = do
  let mymap = Map.fromFoldable [Tuple "one" 1, Tuple "zero" 0, Tuple "three" 3]
  log "Should print (Just 0)"
  logShow $ Map.lookup "zero" mymap
  log "Should print Nothing"
  logShow $ Map.lookup "threeve" mymap

main :: Test Unit
main = do
  partialTest
  totalTest
  insertTest
  deleteTest
  popTest
  alterTest
  fromFoldableTest
