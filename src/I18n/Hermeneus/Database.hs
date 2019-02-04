{-# LANGUAGE TemplateHaskell #-}
module I18n.Hermeneus.Database ( Database
                               , Context
                               , LangInfo
                               , hasTranslation
                               , getLocalizedWord
                               , getLocalizedTemplate
                               , localizeArgument
                               , exampleDB
                               , exampleEn
                               )
where

import Data.Maybe
import qualified Data.List.NonEmpty as NEL
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Data.Aeson.TH

import I18n.Hermeneus.Prim


hasTranslation :: Database -> String -> SentenceKey -> [WordKey] -> Bool
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
getEnglishWordTranslation singular plural = LocalizedWord featureEnv $ NEL.fromList forms
  where
    featureEnv = FeatureEnv M.empty
    forms = [ (FeatureCondition $ M.fromList [(numberFeature, singularValue)], singular)
            , (FeatureCondition $ M.fromList [(numberFeature, pluralValue)], plural)
            ]

getLocalizedWord :: TranslationSet -> WordKey -> Maybe LocalizedWord
getLocalizedWord ts k = M.lookup k $ translationWords ts

getLocalizedTemplate :: TranslationSet -> SentenceKey -> Maybe TranslationTemplate
getLocalizedTemplate ts k = M.lookup k $ translationSentences ts

-- todo: move?
localizeArgument :: TranslationSet -> MessageArg -> LocalizedWord
localizeArgument ts (ArgNumber x) = translateNumber (langInfo ts) x
localizeArgument _ (ArgString x) = nonTranslatedString x
localizeArgument ts (ArgWord s p c) = fromMaybe engWord $ getLocalizedWord ts (WordKey s p c)
  where
    engWord = getEnglishWordTranslation s p
