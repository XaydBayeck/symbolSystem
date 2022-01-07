{-# LANGUAGE LambdaCase #-}

module SymbolSystem.ExprParserMonad where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import SymbolSystem.ExprData (StrictList (Cons, Single))

type Input = String

data ParseErr
  = UnexpectedEof
  | EmptyInput
  | ExepectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  deriving (Eq, Show)

data ParseResult a = Err ParseErr | Res Input a deriving (Eq, Show)

instance Functor ParseResult where
  fmap _ (Err perr) = Err perr
  fmap f (Res i a) = Res i (f a)

newtype Parser a = Prs {parse :: Input -> ParseResult a}

instance Functor Parser where
  fmap f (Prs p) = Prs (fmap f . p)

instance Applicative Parser where
  pure a = Prs $ \input -> Res input a
  pf <*> pa = (>>=) pf $ \f -> fmap f pa

{-- `>>=` 的功能
`p`若解析成功得到`Res i a`则根据上一次解析结果`a`
按照生成规则`f`
生成一个新的解析器`f a`用来解析剩余未解析部分`i`--}
instance Monad Parser where
  (Prs p) >>= f = Prs $ \input -> case p input of
    (Res i a) -> parse (f a) i
    (Err pe) -> Err pe

instance Alternative Parser where
  empty = Prs $ \_ -> Err UnexpectedEof
  (<|>) (Prs pa) (Prs alt) = Prs $ \input ->
    let x = pa input
     in if isErroResult x then alt input else x
    where
      isErroResult (Err _) = True
      isErroResult (Res _ _) = False

instance MonadPlus Parser

(|>) :: Parser a -> Parser a -> Parser a
(Prs p) |> (Prs g) = Prs $ \i0 -> case p i0 of
  (Res _ _) -> g i0
  e@(Err _) -> e

character :: Parser Char
character = Prs $ \case
  (c : cs) -> Res cs c
  _ -> Err UnexpectedEof

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = (>>=) character $ \a ->
  if f a
    then pure a
    else Prs $ \_ -> Err $ UnexpectedChar a
