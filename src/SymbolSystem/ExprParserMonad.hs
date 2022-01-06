{-# LANGUAGE LambdaCase #-}

module SymbolSystem.ExprParserMonad where

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

newtype Parser a = Prs (Input -> ParseResult a)

instance Functor Parser where
  fmap f (Prs p) = Prs (fmap f . p)

instance Applicative Parser where
  pure a = Prs $ \input -> Res input a
  (<*>) pf pa = (>>=) pf $ \f -> fmap f pa

{-- `>>=` 的功能
`p`若解析成功得到`Res i a`则根据上一次解析结果`a`
按照生成规则`f`
生成一个新的解析器`f a`用来解析剩余未解析部分`i`--}
instance Monad Parser where
  (Prs p) >>= f = Prs $ \input -> case p input of
    (Res i a) -> parse (f a) i
    (Err pe) -> Err pe

parse :: Parser a -> Input -> ParseResult a
parse (Prs p) = p

character :: Parser Char
character = Prs parseChar
  where
    parseChar (c : cs) = Res cs c
    parseChar _ = Err UnexpectedEof

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = (>>=) character $ \a ->
  if f a
    then pure a
    else Prs $ \_ -> Err $ UnexpectedChar a

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (Prs pa) (Prs alt) = Prs $ \input ->
  let x = pa input
   in if isErroResult x then alt input else x
  where
    isErroResult (Err _) = True
    isErroResult (Res _ _) = False

anyChar :: Parser Char
anyChar = Prs $ \case
  [] -> Err EmptyInput
  (c : cs) -> Res cs c

(|>) :: a -> (a -> b) -> b
a |> f = f a
