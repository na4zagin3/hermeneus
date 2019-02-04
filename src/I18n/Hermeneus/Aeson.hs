{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module I18n.Hermeneus.Aeson where

import Data.Aeson as JSON
import qualified Data.Char as C
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics
import Text.Parsec

import qualified I18n.Hermeneus.Message as M
import qualified I18n.Hermeneus.NumberHandling as NH
import I18n.Hermeneus.Prim

instance FromJSON TranslationTemplate where
  parseJSON = withText "TranslationTemplate" $ \v ->
    either (fail . show) return $ parse M.parseTranslationTemplate "NumberHandling Expr" v
instance ToJSON TranslationTemplate where
  toJSON = JSON.String . M.printTranslationTemplate

instance FromJSON NH.Expr where
  parseJSON = withText "NumberHandling" $ \v ->
    either (fail . show) return $ parse NH.expr "NumberHandling Expr" v
instance ToJSON NH.Expr where
  toJSON = JSON.String . NH.printExpr

instance FromJSON FeatureEnv where
instance ToJSON FeatureEnv where

instance FromJSON FeatureCondition where
instance ToJSON FeatureCondition where

instance FromJSON LocalizedWord where
instance ToJSON LocalizedWord where

instance FromJSON NumberFeature where
  parseJSON = withObject "NumberFeature" $ \v -> NumberFeature
      <$> v .: "condition"
      <*> v .: "value"
instance ToJSON NumberFeature where
  toJSON nf = object [ "condition" .= numberFExpr nf
                     , "value" .= numberFId nf
                     ]

instance FromJSON NumberHandling where
  parseJSON = withObject "NumberHandling" $ \v -> NumberHandling
      <$> v .: "default"
      <*> v .: "values"
instance ToJSON NumberHandling where
  toJSON nh = object [ "default" .= numberDefaultFeature nh
                     , "values" .= numberExpressions nh
                     ]

instance FromJSON LangInfo where
  parseJSON = withObject "LangInfo" $ \v -> LangInfo
      <$> v .: "number"
instance ToJSON LangInfo where
  toJSON li = object [ "number" .= numberHandling li
                     ]

data SentenceEntry = SentenceEntry
  { sentenceSentence :: String
  , sentenceContext :: String
  , sentenceTranslation :: TranslationTemplate
  } deriving (Read, Show, Generic)

instance FromJSON SentenceEntry where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 8
                                              }
instance ToJSON SentenceEntry where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 8
                                        }

sentenceEntryToKey :: SentenceEntry -> (SentenceKey, TranslationTemplate)
sentenceEntryToKey (SentenceEntry s c t) = (SentenceKey s c, t)
sentenceKeyToEntry :: (SentenceKey, TranslationTemplate) ->  SentenceEntry
sentenceKeyToEntry (SentenceKey s c, t) = SentenceEntry s c t

data WordTranslationEntry = WordTranslationEntry
  { wordTranslationCondition :: FeatureCondition
  , wordTranslationTranslation :: String
  } deriving (Read, Show, Generic)

instance FromJSON WordTranslationEntry where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 15
                                              }
instance ToJSON WordTranslationEntry where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 15

                                        }
wordTranslationEntryToKey :: WordTranslationEntry -> (FeatureCondition, String)
wordTranslationEntryToKey (WordTranslationEntry o t) = (o, t)

wordTranslationKeyToEntry :: (FeatureCondition, String) -> WordTranslationEntry
wordTranslationKeyToEntry (o, t) = WordTranslationEntry o t

data WordEntry = WordEntry
  { wordWord :: String
  , wordWordPlural :: Maybe String
  , wordContext :: String
  , wordController :: FeatureEnv
  , wordTranslations :: NEL.NonEmpty WordTranslationEntry
  } deriving (Read, Show, Generic)

instance FromJSON WordEntry where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 4
                                              }
instance ToJSON WordEntry where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 4

                                        }
wordEntryToKey :: WordEntry -> (WordKey, LocalizedWord)
wordEntryToKey (WordEntry w p c o t) = (WordKey w (fromMaybe "" p) c, LocalizedWord o $ NEL.map wordTranslationEntryToKey t) -- ToDo: Fix this

wordKeyToEntry :: (WordKey, LocalizedWord) -> WordEntry
wordKeyToEntry (WordKey w p c, LocalizedWord o t) = (WordEntry w (f p) c o $ NEL.map wordTranslationKeyToEntry t)
  where
    f "" = Nothing
    f x = Just x

instance FromJSON TranslationSet where
  parseJSON = withObject "TranslationSet" $ \v -> do
      p <- v .: "properties"
      ss <- M.fromList . map sentenceEntryToKey <$> v .: "sentences"
      ws <- M.fromList . map wordEntryToKey <$> v .: "words"
      return $ TranslationSet p ss ws
instance ToJSON TranslationSet where
  toJSON ts = object [ "properties" .= langInfo ts
                     , "sentences" .= (map sentenceKeyToEntry . M.toList $ translationSentences ts)
                     , "words" .= (map wordKeyToEntry . M.toList $ translationWords ts)
                     ]
