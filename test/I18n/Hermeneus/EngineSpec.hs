module I18n.Hermeneus.EngineSpec where

import Data.Either
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import I18n.Hermeneus.Prim

import Test.Tasty.HUnit

import I18n.Hermeneus.Engine

{-# ANN module "HLint: ignore Use camelCase" #-}

featureCondition = FeatureCondition . M.fromList
featureEnv = FeatureEnv . M.fromList

n = featureValueFromString "n"
s = featureValueFromString "s"
d = featureValueFromString "d"

unit_selectWord1, unit_selectWord2, unit_selectWord3 :: IO ()
unit_selectWord1 = selectWord (featureEnv []) [(featureCondition [], "a")] @?= "a"
unit_selectWord2 = selectWord (featureEnv [(n, s)]) [(featureCondition [(n, s)], "a"), (featureCondition [], "b")] @?= "a"
unit_selectWord3 = selectWord (featureEnv [(n, d)]) [(featureCondition [(n, s)], "a"), (featureCondition [], "b")] @?= "b"

unit_matchFeatureConstraint :: IO ()
unit_matchFeatureConstraint = do
  matchFeatureConstraint (featureCondition []) (featureEnv []) @?= True
  matchFeatureConstraint (featureCondition [(n, s)]) (featureEnv []) @?= False
  matchFeatureConstraint (featureCondition []) (featureEnv [(n, s)]) @?= True
  matchFeatureConstraint (featureCondition [(n, s)]) (featureEnv [(n, s)]) @?= True
  matchFeatureConstraint (featureCondition [(n, s)]) (featureEnv [(n, d)]) @?= False

unit_resolveFeatures :: IO ()
unit_resolveFeatures = do
  resolveFeatureConstraintExpr [] (M.fromList []) (FeatureConstraintExpr n (Feature s)) @?= Right (M.fromList [(n, s)])
  resolveFeatureConstraintExpr [featureEnv [(n, s)]] (M.fromList []) (FeatureConstraintExpr n (ConcordWord (PlaceholderNumber 0))) @?= Right (M.fromList [(n, s)])
  resolveFeatureConstraintExpr [] (M.fromList [("w", featureEnv [(n, s)])]) (FeatureConstraintExpr n (ConcordWord (WordRef "w"))) @?= Right (M.fromList [(n, s)])

  isLeft (resolveFeatureConstraintExpr [featureEnv []] (M.fromList []) (FeatureConstraintExpr n (ConcordWord (PlaceholderNumber 0)))) @?= True
  isLeft (resolveFeatureConstraintExpr [] (M.fromList [("w", featureEnv [])]) (FeatureConstraintExpr n (ConcordWord (WordRef "w")))) @?= True


unit_translatePlaceholders :: IO ()
unit_translatePlaceholders = do
  translatePlaceholders [] M.empty [] @?= Right []
  translatePlaceholders [(PlaceholderNumber 0, [])] M.empty [LocalizedWord (featureEnv []) (NEL.fromList [(featureCondition [], "a")])] @?= Right ["a"]
  let placeholders = [(PlaceholderNumber 1, [FeatureConstraintExpr numberFeature (ConcordWord (PlaceholderNumber 0))]), (PlaceholderNumber 0, [])]
  let word1 = LocalizedWord (featureEnv [(numberFeature, singularValue)]) (NEL.fromList [(featureCondition [], "one")])
  let wordCar = LocalizedWord (featureEnv []) (NEL.fromList
                                               [ (featureCondition [(numberFeature, singularValue)], "car")
                                               , (featureCondition [(numberFeature, singularValue)], "cars")
                                               ])
  let argument1 = [ word1, wordCar ]
  translatePlaceholders placeholders M.empty argument1 @?= Right ["one", "car"]


