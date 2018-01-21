module DocTests where

import Control.Concurrent
import Control.Exception
import System.FilePath.Glob
import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit


 -- ToDo: Waiting with semaphore is not beautiful
docTestFile :: IO QSem -> FilePath -> TestTree
docTestFile sem fp = testCase fp $ do
  s <- sem
  bracket_ (waitQSem s) (signalQSem s) $ doctest [fp]

docTestFiles :: [FilePath] -> TestTree
docTestFiles files = withResource (newQSem 1) (const $ return ()) $ \sem -> testGroup "Doc tests" $ map (docTestFile sem) files

test_doctests :: IO TestTree
test_doctests = do
  files <- glob "src/**/*.hs"
  return $ testCase "Doc tests" $ doctest files
  -- return $ withResource (newQSem 1) (const $ return ()) $ \sem -> testGroup "Doc tests" $ map (docTestFile sem) files


