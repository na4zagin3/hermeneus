{-# LANGUAGE OverloadedStrings #-}

module I18n.Hermeneus.Aeson where

import Data.Aeson as JSON
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

instance FromJSON TranslationSet where
  parseJSON = withObject "TranslationSet" $ \v -> TranslationSet
      <$> v .: "properties"
      <*> v .: "sentences"
      <*> v .: "words"
instance ToJSON TranslationSet where
  toJSON ts = object [ "properties" .= langInfo ts
                     , "sentences" .= translationSentences ts
                     , "words" .= translationWords ts
                     ]
