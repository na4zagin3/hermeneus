{-# OPTIONS_GHC -F -pgmF htfpp #-}

module I18n.Hermeneus.Prim where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import           Test.Framework

--
-- Message translation data
--

type FeatureId = String
type FeatureValue = String

type FeaturePair = (FeatureId, FeatureValue)

type FeatureEnv = Map FeatureId FeatureValue
data FeatureCondition = FeatureCondition [FeaturePair]
  deriving (Eq, Ord, Show, Read)

data TranslationTemplate = TranslatedString String
                         | Placeholder Placeholder
  deriving (Eq, Ord, Show, Read)

type Placeholder = (WordReference, [FeatureConstraintExpr])

data WordReference = PlaceholderNumber Integer
                   | WordKey String
  deriving (Eq, Ord, Show, Read)

data FeatureConstraintExpr = FeatureConstraintExpr FeatureId FeatureReferenceExpr
  deriving (Eq, Ord, Show, Read)

data FeatureReferenceExpr = ConcordWord WordReference
                          | Feature FeatureId
  deriving (Eq, Ord, Show, Read)

--
-- Word translation data
--

data LocalizedWord = LocalizedWord FeatureEnv [(FeatureCondition, String)]
  deriving (Eq, Ord, Show, Read)

--
-- Language feature data
--

data LangInfo = LangInfo { numberHandling :: NumberHandling
                         }
  deriving (Eq, Ord, Show, Read)

data NumberHandling = NumberHandling FeatureId [(NumberHandlingCond, FeatureId)]
  deriving (Eq, Ord, Show, Read)

data NumberHandlingCond = NHEqual NumberHandlingExpr NumberHandlingExpr
                        | NHGreater NumberHandlingExpr NumberHandlingExpr
                        | NHGreaterEqual NumberHandlingExpr NumberHandlingExpr
                        | NHLess NumberHandlingExpr NumberHandlingExpr
                        | NHLessEqual NumberHandlingExpr NumberHandlingExpr
  deriving (Eq, Ord, Show, Read)

data NumberHandlingExpr = NHTarget
                        | NHNum Integer
                        | NHAdd NumberHandlingExpr NumberHandlingExpr
                        | NHSub NumberHandlingExpr NumberHandlingExpr
                        | NHMul NumberHandlingExpr NumberHandlingExpr
                        | NHDiv NumberHandlingExpr NumberHandlingExpr
                        | NHMod NumberHandlingExpr NumberHandlingExpr
  deriving (Eq, Ord, Show, Read)

numberHandlingEn, numberHandlingJa, numberHandlingGrc :: NumberHandling
numberHandlingEn = NumberHandling "plural" [(NHEqual NHTarget $ NHNum 1, "single")]
numberHandlingJa = NumberHandling "none" []
numberHandlingGrc = NumberHandling "plural" [(NHEqual NHTarget $ NHNum 1, "single"), (NHEqual NHTarget $ NHNum 2, "dual")]

determineNumber :: Integer -> NumberHandling -> FeatureId
determineNumber x (NumberHandling d cs) = determineNumberWithCond x d cs

determineNumberWithCond :: Integer -> FeatureId -> [(NumberHandlingCond, FeatureId)] -> FeatureId
determineNumberWithCond _ d [] = d
determineNumberWithCond x d ((c, f) : cs) | evalNumberHandlingCond x c = f
                                          | otherwise = determineNumberWithCond x d cs

evalNumberHandlingCond :: Integer -> NumberHandlingCond -> Bool
evalNumberHandlingCond x (NHEqual a b) = evalNumberHandlingExpr x a == evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHGreater a b) = evalNumberHandlingExpr x a > evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHGreaterEqual a b) = evalNumberHandlingExpr x a >= evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHLess a b) = evalNumberHandlingExpr x a < evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHLessEqual a b) = evalNumberHandlingExpr x a <= evalNumberHandlingExpr x b

evalNumberHandlingExpr :: Integer -> NumberHandlingExpr -> Integer
evalNumberHandlingExpr x NHTarget = x
evalNumberHandlingExpr _ (NHNum n) = n
evalNumberHandlingExpr x (NHAdd a b) = evalNumberHandlingExpr x a + evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHSub a b) = evalNumberHandlingExpr x a - evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHMul a b) = evalNumberHandlingExpr x a * evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHDiv a b) = evalNumberHandlingExpr x a `div` evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHMod a b) = evalNumberHandlingExpr x a `mod` evalNumberHandlingExpr x b
