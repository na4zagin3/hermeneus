{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module I18n.Hermeneus.Database ( Database
                               , Context
                               , LangInfo
                               , hasTranslation
                               , getLocalizedWord
                               , getLocalizedTemplate
                               , localizeArgument
                               , exampleDB
                               , exampleEn
                               , htf_thisModulesTests
                               )
where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import           Test.Framework

import Data.Aeson.TH

import I18n.Hermeneus.Prim


hasTranslation :: Database -> String -> (String, Context) -> [(String, String, Context)] -> Bool
hasTranslation db loc s ws = fromMaybe False $ do
  ts <- loc `M.lookup` db
  _ <- s `M.lookup` translationSentences ts
  return $ S.fromList ws `S.isSubsetOf` M.keysSet (translationWords ts)

exampleDB = M.fromList [("en", exampleEn)]

exampleEn = TranslationSet { langInfo = LangInfo { numberHandling = numberHandlingEn }
                           , translationSentences = M.fromList [ ]
                           , translationWords = M.fromList [ ]

                           }

getEnglishWordTranslation :: String -> String -> LocalizedWord
getEnglishWordTranslation singular plural = LocalizedWord featureEnv forms
  where
    featureEnv = M.empty
    forms = [ (FeatureCondition [(numberFeature, singularValue)], singular)
            , (FeatureCondition [(numberFeature, pluralValue)], plural)
            ]

getLocalizedWord :: TranslationSet -> (String, String, Context) -> Maybe LocalizedWord
getLocalizedWord ts k = M.lookup k $ translationWords ts

getLocalizedTemplate :: TranslationSet -> (String, Context) -> Maybe [TranslationTemplate]
getLocalizedTemplate ts k = M.lookup k $ translationSentences ts

-- todo: move?
localizeArgument :: TranslationSet -> MessageArg -> LocalizedWord
localizeArgument ts (ArgNumber x) = translateNumber (langInfo ts) x
localizeArgument _ (ArgString x) = nonTranslatedString x
localizeArgument ts (ArgWord s p c) = fromMaybe engWord $ getLocalizedWord ts (s, p, c) 
  where
    engWord = getEnglishWordTranslation s p
