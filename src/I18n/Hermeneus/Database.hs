{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module I18n.Hermeneus.Database ( Database
                               , TranslationSet(langInfo, sentences, words)
                               , Context
                               , LangInfo
                               , hasTranslation
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
import I18n.Hermeneus.Engine

type Context = String
data TranslationSet = TranslationSet { langInfo :: LangInfo
                                     , sentences :: Map (String, Context) [TranslationTemplate]
                                     , words :: Map (String, Context) LocalizedWord
                                     }
  deriving (Eq, Ord, Show, Read)

data LangInfo = LangInfo
  deriving (Eq, Ord, Show, Read)

-- ToDo: handle locale string
type Database = Map String TranslationSet

hasTranslation :: Database -> String -> (String, Context) -> [(String, Context)] -> Bool
hasTranslation db loc s ws = fromMaybe False $ do
  ts <- loc `M.lookup` db
  _ <- s `M.lookup` sentences ts
  return $ S.fromList ws `S.isSubsetOf` M.keysSet (sentences ts)
  
