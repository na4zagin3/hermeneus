{-# OPTIONS_GHC -F -pgmF htfpp #-}

module I18n.Hermeneus where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import           Test.Framework

import I18n.Hermeneus.Prim
import I18n.Hermeneus.Engine

data MessageArgument = RawString String
                     | Localized LocalizedWord
  deriving (Eq, Ord, Show, Read)

trw :: String -> LocalizedWord
trw = undefined

trs :: String -> [MessageArgument] -> LocalizedWord
trs = undefined

