module Shopping where

import Control.Exception
import Control.Monad
import qualified Data.Map as M
import qualified Data.Yaml as Y
import I18n.Hermeneus.Aeson
import I18n.Hermeneus.Engine (translateMessage)
import I18n.Hermeneus.Prim
import System.FilePath
import System.FilePath.Glob

import Test.Tasty.HUnit

resourceDir :: FilePath
resourceDir = "test-resource/Shopping"

translationResource :: IO Database
translationResource = do
  files <- globDir1 (compile "*.yaml") resourceDir
  translationSets <- forM files $ \fp -> do
    let locale = takeFileName $ dropExtensions fp
    s <- either throw id <$> Y.decodeFileEither fp
    return (locale, s)
  return $ M.fromList translationSets

unit_translationResource = do
  db <- translationResource
  M.null db @?= False

unit_translations = do
  db <- translationResource
  -- translateMessage :: Database -> Locale -> MessageKey -> [MessageArg] -> Either String String
  let context = "com.example.shopping.buy"
  let msgBought l n w p = translateMessage db l (MessageKey "Bought {0} {1:number#0}." context) [ArgNumber n, ArgWord w p context]
  msgBought "en" 1 "car" "cars" @?= Right "Bought 1 car."
  msgBought "en" 2 "car" "cars" @?= Right "Bought 2 cars." -- ToDo: Fix this
  msgBought "en" 2 "book" "books" @?= Right "Bought 2 books."
  -- ToDo: Fix those cases
  -- msgBought "ja" 1 "car" "cars" @?= Right "1台の車を買った。"
  -- msgBought "ja" 1 "car" "cars" @?= Right "1台の車を買った。"
  msgBought "ja" 1 "book" "books" @?= Right "1冊の本を買った。"
