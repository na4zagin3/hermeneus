module I18n.Hermeneus.EngineSpec where

import qualified Data.Map as M
import I18n.Hermeneus.Prim

import Test.Tasty.HUnit

import I18n.Hermeneus.Engine

{-# ANN module "HLint: ignore Use camelCase" #-}

featureCondition = FeatureCondition . M.fromList

n = featureValueFromString "n"
s = featureValueFromString "s"
d = featureValueFromString "d"

unit_selectWord1, unit_selectWord2, unit_selectWord3 :: IO ()
unit_selectWord1 = selectWord (M.fromList []) [(featureCondition [], "a")] @?= "a"
unit_selectWord2 = selectWord (M.fromList [(n, s)]) [(featureCondition [(n, s)], "a"), (featureCondition [], "b")] @?= "a"
unit_selectWord3 = selectWord (M.fromList [(n, d)]) [(featureCondition [(n, s)], "a"), (featureCondition [], "b")] @?= "b"

unit_matchFeatureConstraint :: IO ()
unit_matchFeatureConstraint = do
  matchFeatureConstraint (featureCondition []) (M.fromList []) @?= True
  matchFeatureConstraint (featureCondition [(n, s)]) (M.fromList []) @?= False
  matchFeatureConstraint (featureCondition []) (M.fromList [(n, s)]) @?= True
  matchFeatureConstraint (featureCondition [(n, s)]) (M.fromList [(n, s)]) @?= True
  matchFeatureConstraint (featureCondition [(n, s)]) (M.fromList [(n, d)]) @?= False

-- unit_resolveFeatures :: IO ()
-- unit_resolveFeatures = do
--   resolveFeatureConstraintExpr (WordKey "a") (M.fromList [("a", M.fromList [("b", "c")])]) @?= [("b", "c")]
--   resolveFeatureConstraintExpr (WordKey "a") $ (M.fromList [("a", M.fromList [("b", "c"), ("d", "e")])]) @?= [("b", "c")]


unit_translatePlaceholders :: IO ()
unit_translatePlaceholders = do
  translatePlaceholders [] M.empty [] @?= Right []
  translatePlaceholders [(PlaceholderNumber 0, [])] M.empty [LocalizedWord M.empty [(featureCondition [], "a")]] @?= Right ["a"]
  let placeholders = [(PlaceholderNumber 1, [FeatureConstraintExpr numberFeature (ConcordWord (PlaceholderNumber 0))]), (PlaceholderNumber 0, [])]
  let word1 = LocalizedWord (M.fromList [(numberFeature, singularValue)]) [(featureCondition [], "one")]
  let wordCar = LocalizedWord (M.fromList []) [ (featureCondition [(numberFeature, singularValue)], "car")
                                              , (featureCondition [(numberFeature, singularValue)], "cars")
                                              ]
  let argument1 = [ word1, wordCar ]
  translatePlaceholders placeholders M.empty argument1 @?= Right ["one", "car"]


