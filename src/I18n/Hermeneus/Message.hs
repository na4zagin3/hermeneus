{-# LANGUAGE FlexibleContexts #-}

module I18n.Hermeneus.Message where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Text.Parsec

import I18n.Hermeneus.Prim


parseTranslationMessage :: Stream s m Char => ParsecT s u m [TranslationTemplate]
parseTranslationMessage = many parseTranslationTemplate

parseTranslationTemplate :: Stream s m Char => ParsecT s u m TranslationTemplate
parseTranslationTemplate = parsePlaceholder <|> parseTranslatedString

parsePlaceholder :: Stream s m Char => ParsecT s u m TranslationTemplate
parsePlaceholder = do
  string "{"
  wRef <- parseWordReference
  exprs <- option [] $ string ":" *> many parseFeatureConstraintExpr
  string "}"
  return $ Placeholder (wRef, exprs)

parseTranslatedString :: Stream s m Char => ParsecT s u m TranslationTemplate
parseTranslatedString = fmap TranslatedString $ many1 $ noneOf "{}"

parseWordReference :: Stream s m Char => ParsecT s u m WordReference
parseWordReference = parseWord <|> parseNumber
  where
    parseNumber = PlaceholderNumber . read <$> many1 digit
    parseWord = do
      string "*"
      fmap WordKey . many $ noneOf ":"

parseFeatureConstraintExpr :: Stream s m Char => ParsecT s u m FeatureConstraintExpr
parseFeatureConstraintExpr = do
  i <- parseFeatureId
  expr <- parseFeatureReferenceExpr
  return $ FeatureConstraintExpr i expr

parseFeatureId :: Stream s m Char => ParsecT s u m FeatureId
parseFeatureId = many1 alphaNum

parseFeatureValue :: Stream s m Char => ParsecT s u m FeatureValue
parseFeatureValue = many1 alphaNum

parseFeatureReferenceExpr :: Stream s m Char => ParsecT s u m FeatureReferenceExpr
parseFeatureReferenceExpr = parseConcordWord <|> parseFeature
  where
    parseConcordWord = do
      string "#"
      wordRef <- parseWordReference
      return $ ConcordWord wordRef
    parseFeature = do
      string "="
      value <- parseFeatureValue
      return $ Feature value
