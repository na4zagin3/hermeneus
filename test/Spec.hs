{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} I18n.Hermeneus
import {-@ HTF_TESTS @-} I18n.Hermeneus.Prim
import {-@ HTF_TESTS @-} I18n.Hermeneus.Engine
import {-@ HTF_TESTS @-} I18n.Hermeneus.Message
import {-@ HTF_TESTS @-} I18n.Hermeneus.Database

main :: IO ()
main = htfMain htf_importedTests
