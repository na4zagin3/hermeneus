module I18n.Hermeneus.EngineSpec where

import qualified Data.Map as M
import I18n.Hermeneus.Prim

import Test.Tasty.HUnit

import I18n.Hermeneus.Engine

{-# ANN module "HLint: ignore Use camelCase" #-}

n = featureValueFromString "n"
s = featureValueFromString "s"
d = featureValueFromString "d"

unit_selectWord1, unit_selectWord2, unit_selectWord3 :: IO ()
unit_selectWord1 = selectWord (M.fromList []) [(FeatureCondition [], "a")] @?= "a"
unit_selectWord2 = selectWord (M.fromList [(n, s)]) [(FeatureCondition [(n, s)], "a"), (FeatureCondition [], "b")] @?= "a"
unit_selectWord3 = selectWord (M.fromList [(n, d)]) [(FeatureCondition [(n, s)], "a"), (FeatureCondition [], "b")] @?= "b"

unit_matchFeatureConstraint :: IO ()
unit_matchFeatureConstraint = do
  matchFeatureConstraint (FeatureCondition []) (M.fromList []) @?= True
  matchFeatureConstraint (FeatureCondition [(n, s)]) (M.fromList []) @?= False
  matchFeatureConstraint (FeatureCondition []) (M.fromList [(n, s)]) @?= True
  matchFeatureConstraint (FeatureCondition [(n, s)]) (M.fromList [(n, s)]) @?= True
  matchFeatureConstraint (FeatureCondition [(n, s)]) (M.fromList [(n, d)]) @?= False

-- unit_resolveFeatures :: IO ()
-- unit_resolveFeatures = do
--   resolveFeatureConstraintExpr (WordKey "a") (M.fromList [("a", M.fromList [("b", "c")])]) @?= [("b", "c")]
--   resolveFeatureConstraintExpr (WordKey "a") $ (M.fromList [("a", M.fromList [("b", "c"), ("d", "e")])]) @?= [("b", "c")]


unit_translatePlaceholders :: IO ()
unit_translatePlaceholders = do
  translatePlaceholders [] M.empty [] @?= Right []
  translatePlaceholders [(PlaceholderNumber 0, [])] M.empty [LocalizedWord M.empty [(FeatureCondition [], "a")]] @?= Right ["a"]
  let placeholders = [(PlaceholderNumber 1, [FeatureConstraintExpr numberFeature (ConcordWord (PlaceholderNumber 0))]), (PlaceholderNumber 0, [])]
  let word1 = LocalizedWord (M.fromList [(numberFeature, singularValue)]) [(FeatureCondition [], "one")]
  let wordCar = LocalizedWord (M.fromList []) [ (FeatureCondition [(numberFeature, singularValue)], "car")
                                                 , (FeatureCondition [(numberFeature, singularValue)], "cars")
                                                 ]
  let argument1 = [ word1, wordCar ]
  translatePlaceholders placeholders M.empty argument1 @?= Right ["one", "car"]


