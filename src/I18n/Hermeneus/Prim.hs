{-# LANGUAGE DeriveGeneric #-}

module I18n.Hermeneus.Prim where

import Data.Fixed
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)

import qualified I18n.Hermeneus.NumberHandling as NH

--
-- Message translation data
--
type FeatureId = NonEmptyString
type FeatureValue = NonEmptyString

{-
nonEmptyStringToString :: NonEmptyString -> String
nonEmptyStringFromString :: String -> NonEmptyString
featureIdFromString :: String -> FeatureId
featureValueFromString :: String -> FeatureValue

type NonEmptyString = NonEmpty Char

nonEmptyStringToString = NEL.toList
nonEmptyStringFromString = NEL.fromList
featureIdFromString = NEL.fromList
featureValueFromString = NEL.fromList
-}

type NonEmptyString = String

nonEmptyStringToString = id
nonEmptyStringFromString = id
featureIdFromString = id
featureValueFromString = id

type FeaturePair = (FeatureId, FeatureValue)

type FeatureEnv = Map FeatureId FeatureValue
data FeatureCondition = FeatureCondition [FeaturePair]
  deriving (Eq, Ord, Show, Read)

newtype TranslationTemplate = TranslationTemplate [TranslationHank]
  deriving (Eq, Ord, Show, Read, Generic)

normalizeTranslationTemplate (TranslationTemplate ts) = TranslationTemplate $ normalize ts
  where
    normalize [] = []
    normalize (TranslatedString s1 : TranslatedString s2 : ts) = normalize $ TranslatedString (s1 <> s2) : ts
    normalize (t : ts) = t : normalize ts

data TranslationHank = TranslatedString NonEmptyString
                     | Placeholder Placeholder
  deriving (Eq, Ord, Show, Read, Generic)

type Placeholder = (WordReference, [FeatureConstraintExpr])

data WordReference = PlaceholderNumber Integer
                   | WordKey String
  deriving (Eq, Ord, Show, Read, Generic)

data FeatureConstraintExpr = FeatureConstraintExpr FeatureId FeatureReferenceExpr
  deriving (Eq, Ord, Show, Read, Generic)

data FeatureReferenceExpr = ConcordWord WordReference
                          | Feature FeatureId
  deriving (Eq, Ord, Show, Read, Generic)

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
                                     , translationSentences :: Map (String, Context) TranslationTemplate
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
data MessageArg = ArgNumber Integer -- ToDo: Support decimals
                | ArgString String
                | ArgWord String String Context
  deriving (Eq, Ord, Read, Show)

data NumberHandling = NumberHandling { numberDefaultFeature :: FeatureId -- ToDo: Isn't it FeatureValue?
                                     , numberExpressions :: [(NH.Expr, FeatureId)] -- ToDo: Isn't it FeatureValue?
                                     }
  deriving (Eq, Ord, Show, Read)

numberHandlingEn, numberHandlingJa, numberHandlingGrc :: NumberHandling
numberHandlingEn = NumberHandling pluralValue [(NH.EEq NH.ETarget $ NH.ENumber 1, singularValue)]
numberHandlingJa = NumberHandling noneValue []
numberHandlingGrc = NumberHandling pluralValue [(NH.EEq NH.ETarget $ NH.ENumber 1, singularValue), (NH.EEq NH.ETarget $ NH.ENumber 2, dualValue)]

-- Todo locale specific number format
translateNumber :: LangInfo -> Integer -> LocalizedWord
translateNumber li x = LocalizedWord (M.singleton numberFeature $ determineNumber x $ numberHandling li) [(FeatureCondition [], show x)]

determineNumber :: Integer -> NumberHandling -> FeatureId
determineNumber x (NumberHandling d cs) = determineNumberWithCond x d cs

determineNumberWithCond :: Integer -> FeatureId -> [(NH.Expr, FeatureId)] -> FeatureId
determineNumberWithCond _ d [] = d
determineNumberWithCond x d ((c, f) : cs) | NH.evalCond x c = f
                                          | otherwise = determineNumberWithCond x d cs

--
-- Language feature data
--

data LangInfo = LangInfo { numberHandling :: NumberHandling
                         -- ToDo: Static check of features.
                         }
  deriving (Eq, Ord, Show, Read)

--
-- Translation words
--

nonTranslatedString :: String -> LocalizedWord
nonTranslatedString x = LocalizedWord M.empty [(FeatureCondition [], x)]

genderFeature, numberFeature :: FeatureId
genderFeature = featureIdFromString "gender"
numberFeature = featureIdFromString "number"

singularValue, pluralValue, noneValue, dualValue :: FeatureValue
masculineValue, feminineValue, neuterValue :: FeatureValue
masculineValue = featureValueFromString "masculine"
feminineValue = featureValueFromString "feminine"
neuterValue = featureValueFromString "neuter"

singularValue = featureValueFromString "singular"
pluralValue = featureValueFromString "plural"
noneValue = featureValueFromString "none"
dualValue = featureValueFromString "dual"
