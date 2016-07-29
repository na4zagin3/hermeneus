{-# OPTIONS_GHC -F -pgmF htfpp #-}

module I18n.Hermeneus.Prim where

import Data.Fixed
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
-- Database
--

type Context = String
data TranslationSet = TranslationSet { langInfo :: LangInfo
                                     , translationSentences :: Map (String, Context) [TranslationTemplate]
                                     , translationWords :: Map (String, String, Context) LocalizedWord
                                     }
  deriving (Eq, Ord, Show, Read)

type Locale = String

-- ToDo: handle locale string
type Database = Map Locale TranslationSet

--
-- Input / Output
--
data MessageKey = MessageKey String Context
  deriving (Eq, Ord, Read, Show)
data MessageArg = ArgNumber Double
                | ArgString String
                | ArgWord String String Context
  deriving (Eq, Ord, Read, Show)

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
                        | NHNum Rational
                        | NHAdd NumberHandlingExpr NumberHandlingExpr
                        | NHSub NumberHandlingExpr NumberHandlingExpr
                        | NHMul NumberHandlingExpr NumberHandlingExpr
                        | NHDiv NumberHandlingExpr NumberHandlingExpr
                        | NHMod NumberHandlingExpr NumberHandlingExpr
  deriving (Eq, Ord, Show, Read)

numberHandlingEn, numberHandlingJa, numberHandlingGrc :: NumberHandling
numberHandlingEn = NumberHandling pluralValue [(NHEqual NHTarget $ NHNum 1, singularValue)]
numberHandlingJa = NumberHandling noneValue []
numberHandlingGrc = NumberHandling pluralValue [(NHEqual NHTarget $ NHNum 1, singularValue), (NHEqual NHTarget $ NHNum 2, dualValue)]

-- Todo locale specific number format
translateNumber :: (Num a, Eq a, Ord a, Real a, Fractional a, Show a)=> LangInfo -> a -> LocalizedWord
translateNumber li x = LocalizedWord (M.singleton "number" $ determineNumber x $ numberHandling li) [(FeatureCondition [], show x)]

determineNumber :: (Num a, Eq a, Ord a, Real a, Fractional a)=> a -> NumberHandling -> FeatureId
determineNumber x (NumberHandling d cs) = determineNumberWithCond x d cs

determineNumberWithCond :: (Num a, Eq a, Ord a, Real a, Fractional a)=> a -> FeatureId -> [(NumberHandlingCond, FeatureId)] -> FeatureId
determineNumberWithCond _ d [] = d
determineNumberWithCond x d ((c, f) : cs) | evalNumberHandlingCond x c = f
                                          | otherwise = determineNumberWithCond x d cs

evalNumberHandlingCond :: (Num a, Eq a, Ord a, Real a, Fractional a)=> a -> NumberHandlingCond -> Bool
evalNumberHandlingCond x (NHEqual a b) = evalNumberHandlingExpr x a == evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHGreater a b) = evalNumberHandlingExpr x a > evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHGreaterEqual a b) = evalNumberHandlingExpr x a >= evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHLess a b) = evalNumberHandlingExpr x a < evalNumberHandlingExpr x b
evalNumberHandlingCond x (NHLessEqual a b) = evalNumberHandlingExpr x a <= evalNumberHandlingExpr x b

evalNumberHandlingExpr :: (Num a, Real a, Fractional a)=> a -> NumberHandlingExpr -> a
evalNumberHandlingExpr x NHTarget = x
evalNumberHandlingExpr _ (NHNum n) = fromRational n
evalNumberHandlingExpr x (NHAdd a b) = evalNumberHandlingExpr x a + evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHSub a b) = evalNumberHandlingExpr x a - evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHMul a b) = evalNumberHandlingExpr x a * evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHDiv a b) = evalNumberHandlingExpr x a / evalNumberHandlingExpr x b
evalNumberHandlingExpr x (NHMod a b) = evalNumberHandlingExpr x a `mod'` evalNumberHandlingExpr x b


--
-- Translation words
--

nonTranslatedString :: String -> LocalizedWord
nonTranslatedString x = LocalizedWord M.empty [(FeatureCondition [], x)]

numberFeature, singularValue, pluralValue, noneValue, dualValue :: String
numberFeature = "number"
singularValue = "singular"
pluralValue = "plural"
noneValue = "none"
dualValue = "dual"
