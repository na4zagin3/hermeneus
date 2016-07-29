{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} I18n.Hermeneus

main :: IO ()
main = htfMain htf_importedTests
