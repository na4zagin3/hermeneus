module I18n.Hermeneus.Prim where

import Data.Fixed
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import qualified I18n.Hermeneus.NumberHandling as NH

--
-- Message translation data
--

type FeatureId = String
type FeatureValue = String

type FeaturePair = (FeatureId, FeatureValue)

type FeatureEnv = Map FeatureId FeatureValue
data FeatureCondition = FeatureCondition [FeaturePair]
  deriving (Eq, Ord, Show, Read)

newtype TranslationTemplate = TranslationTemplate [TranslationHank]
  deriving (Eq, Ord, Show, Read)

data TranslationHank = TranslatedString String
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
translateNumber li x = LocalizedWord (M.singleton "number" $ determineNumber x $ numberHandling li) [(FeatureCondition [], show x)]

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

numberFeature, singularValue, pluralValue, noneValue, dualValue :: String
numberFeature = "number"
singularValue = "singular"
pluralValue = "plural"
noneValue = "none"
dualValue = "dual"
