{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}
module I18n.Hermeneus.NumberHandling where

import Control.Applicative ((<|>))
import Data.Data
import Data.Generics
import Data.Monoid
import Data.String (IsString, fromString)
import Text.Parsec (parse)
import qualified Text.Parsec as P
import Text.Parser.Combinators ((<?>))
import Text.Parser.Expression (Assoc(..), Operator, buildExpressionParser)
import qualified Text.Parser.Expression as E
import Text.Parser.Token (TokenParsing, integer, natural, parens, reserve, textSymbol)
import Text.Parser.Token.Style (emptyOps)
-- import GHC.Generics (Generic)
import Data.Typeable
import GHC.Generics


data Expr = ETarget
          | ENumber Integer
          | ENeg Expr
          | EAdd Expr Expr
          | ESub Expr Expr
          | EMul Expr Expr
          | EDiv Expr Expr
          | EMod Expr Expr
          | EEq  Expr Expr
          | ENeq Expr Expr
          | ELt  Expr Expr
          | ELe  Expr Expr
          | EGt  Expr Expr
          | EGe  Expr Expr
          | EAnd Expr Expr
          | EOr  Expr Expr
          | ENot Expr
  deriving (Show, Read, Ord, Eq, GHC.Generics.Generic, Typeable, Data)

-- | Evaluate a conditional expression
evalCond :: Integer -> Expr -> Bool
evalCond n e = evalExpr n e /= 0

-- | Evaluate a numerical expression
evalExpr :: Integer -> Expr -> Integer
evalExpr n ETarget = n
evalExpr _ (ENumber n) = n
evalExpr n (ENeg e1   ) = - evalExpr n e1
evalExpr n (EAdd e1 e2) = evalExpr n e1 + evalExpr n e2
evalExpr n (ESub e1 e2) = evalExpr n e1 - evalExpr n e2
evalExpr n (EMul e1 e2) = evalExpr n e1 * evalExpr n e2
evalExpr n (EDiv e1 e2) = evalExpr n e1 `div` evalExpr n e2
evalExpr n (EMod e1 e2) = evalExpr n e1 `mod` evalExpr n e2
evalExpr n (EEq  e1 e2) = boolToInt $ evalExpr n e1 == evalExpr n e2
evalExpr n (ENeq e1 e2) = boolToInt $ evalExpr n e1 /= evalExpr n e2
evalExpr n (ELt  e1 e2) = boolToInt $ evalExpr n e1 <  evalExpr n e2
evalExpr n (ELe  e1 e2) = boolToInt $ evalExpr n e1 <= evalExpr n e2
evalExpr n (EGt  e1 e2) = boolToInt $ evalExpr n e1 >= evalExpr n e2
evalExpr n (EGe  e1 e2) = boolToInt $ evalExpr n e1 >  evalExpr n e2
evalExpr n (EAnd e1 e2) = boolToInt $ evalCond n e1 && evalCond n e2
evalExpr n (EOr  e1 e2) = boolToInt $ evalCond n e1 || evalCond n e2
evalExpr n (ENot e1   ) = boolToInt . not $ evalCond n e1

meltUnaryMinus :: Expr -> Expr
meltUnaryMinus = everywhere (mkT replace)
      where
      replace (ENeg (ENumber x)) | x > 0 = ENumber $ - x
      replace e = e

expandUnaryMinus :: Expr -> Expr
expandUnaryMinus = everywhere (mkT replace)
      where
      replace (ENumber x) | x < 0 = ENeg (ENumber (-x))
      replace e = e

normalize :: Expr -> Expr
normalize = expandUnaryMinus

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

intToBool :: Integer -> Bool
intToBool 0 = False
intToBool _ = True

isLiteral :: Expr -> Bool
isLiteral ETarget = True
isLiteral (ENumber _) = True
isLiteral _ = False

withParen :: (IsString s, Monoid s) => Bool -> s -> s
withParen True s = "(" <> s <> ")"
withParen False s = "(" <> s <> ")"

-- | Print expression
--
-- ToDo: Show parens only when necessary
printExpr :: (IsString s, Monoid s) => Expr -> s
printExpr = printExpr' 0

-- | Print expression
--
-- ToDo: Show parens only when necessary
printExpr' :: (IsString s, Monoid s) => Int -> Expr -> s
printExpr' _ ETarget = "n"
printExpr' _ (ENumber n) = fromString $ show n
printExpr' _ (ENeg e) = printUni "-" e
printExpr' _ (ENot e) = printUni "!"  e
printExpr' _ (EMul e1 e2) = printBin "*"  e1 e2
printExpr' _ (EDiv e1 e2) = printBin "/"  e1 e2
printExpr' _ (EMod e1 e2) = printBin "%"  e1 e2
printExpr' _ (EAdd e1 e2) = printBin "+"  e1 e2
printExpr' _ (ESub e1 e2) = printBin "-"  e1 e2
printExpr' _ (EEq  e1 e2) = printBin "==" e1 e2
printExpr' _ (ENeq e1 e2) = printBin "!=" e1 e2
printExpr' _ (EGt  e1 e2) = printBin ">"  e1 e2
printExpr' _ (EGe  e1 e2) = printBin ">=" e1 e2
printExpr' _ (ELt  e1 e2) = printBin "<"  e1 e2
printExpr' _ (ELe  e1 e2) = printBin "<=" e1 e2
printExpr' _ (EOr  e1 e2) = printBin "||" e1 e2
printExpr' _ (EAnd e1 e2) = printBin "&&" e1 e2

printUni :: (IsString s, Monoid s) => s -> Expr -> s
printUni op e = mconcat [op, withParen True $ printExpr' 0 e]

printBin :: (IsString s, Monoid s) => s -> Expr -> Expr -> s
printBin op e1 e2 = withParen True $ mconcat [printExpr' 0 e1, " ", op, " ", printExpr' 0 e2]


-- | Parse an expression
expr   :: (Monad m, TokenParsing m) => m Expr
expr    = buildExpressionParser table term
        <?> "expression"

term   :: (Monad m, TokenParsing m) => m Expr
term    =  parens expr
        <|> (textSymbol "n" >> return ETarget)
        -- <|> (ENumber <$> natural)
        <|> (ENumber <$> natural)
        <?> "simple expression"

table  :: (Monad m, TokenParsing m) => [[Operator m Expr]]
table   = [ [ prefix "-" ENeg
            , prefix "+" id
            , prefix "!" ENot
            ]
          -- , [postfix "++" (+1)]
          , [ binary "*" EMul AssocLeft, binary "/" EDiv AssocLeft , binary "%" EMod AssocLeft ]
          , [ binary "+" EAdd AssocLeft, binary "-" ESub AssocLeft ]
          , [ binary "==" EEq AssocLeft, binary "!=" ENeq AssocLeft
            , binary "<"  ELt AssocLeft, binary "<=" ELe  AssocLeft
            , binary ">"  EGt AssocLeft, binary ">=" EGe  AssocLeft
            ]
          , [ binary "&&" EAnd AssocLeft ]
          , [ binary "||" EOr AssocLeft ]
          ]

binary name fun = E.Infix (fun <$ reservedOp name)
prefix  name fun       = E.Prefix (fun <$ reservedOp name)
postfix name fun       = E.Postfix (fun <$ reservedOp name)

reservedOp = reserve emptyOps
