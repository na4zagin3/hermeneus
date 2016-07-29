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


hasTranslation :: Database -> String -> (String, Context) -> [(String, Context)] -> Bool
hasTranslation db loc s ws = fromMaybe False $ do
  ts <- loc `M.lookup` db
  _ <- s `M.lookup` sentences ts
  return $ S.fromList ws `S.isSubsetOf` M.keysSet (sentences ts)

exampleEn = TranslationSet { langInfo = LangInfo { numberHandling = numberHandlingEn }
                           }
