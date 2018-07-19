module Example.Shopping where

import Control.Exception
import Control.Monad
import qualified Data.Map as M
import qualified Data.Yaml as Y
import I18n.Hermeneus.Aeson ()
import I18n.Hermeneus.Engine (translateMessage)
import I18n.Hermeneus.Prim
import System.FilePath
import System.FilePath.Glob

import Test.Tasty.HUnit

resourceDir :: FilePath
resourceDir = "test-resource/Example/Shopping"

translationResource :: IO Database
translationResource = do
  files <- globDir1 (compile "*.yaml") resourceDir
  translationSets <- forM files $ \fp -> do
    let locale = takeFileName $ dropExtensions fp
    s <- either throw id <$> Y.decodeFileEither fp
    return (locale, s)
  return $ M.fromList translationSets

unit_translationResource :: IO ()
unit_translationResource = do
  db <- translationResource
  M.null db @?= False

wordCar  = ArgWord "car"  "cars" "com.example.shopping.buy"
wordBook = ArgWord "book" "books" "com.example.shopping.buy"
sentenceBought = MessageKey "Bought {0} {1:number#0}." "com.example.shopping.buy"

unit_translations :: IO ()
unit_translations = do
  db <- translationResource
  let msgBought l n w = translateMessage db l sentenceBought [ArgNumber n, w]
  msgBought "en" 1 wordCar @?= Right "Bought 1 car."
  msgBought "en" 2 wordCar @?= Right "Bought 2 cars." -- ToDo: Fix this
  msgBought "en" 2 wordBook @?= Right "Bought 2 books."
  msgBought "ja" 1 wordCar @?= Right "1台の車を買った。"
  msgBought "ja" 2 wordCar @?= Right "2台の車を買った。"
  msgBought "ja" 1 wordBook @?= Right "1冊の本を買った。"
