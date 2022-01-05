module SymbolSystem.ExprParserMonad where

type Input = String

data ParseErr
  = UnexpectedEof
  | ExepectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  deriving (Eq, Show)

data ParseResult a = Err ParseErr | Res Input a deriving (Eq, Show)

instance Functor ParseResult where
  fmap _ (Err perr) = Err perr
  fmap f (Res i a) = Res i (f a)

newtype Parser a = Prs (Input -> ParseResult a)

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

instance Functor Parser where
  fmap f (Prs p) = Prs (fmap f . p)

instance Applicative Parser where
  pure a = Prs $ \input -> Res input a
  (<*>) (Prs f) p = _todo

instance Monad Parser where
  (>>=) (Prs p) f = Prs $ \input -> onResult (p input) (\input1 a -> parse (f a) input1)
    where
      onResult (Res i a) k = k i a
      onResult (Err pe) _ = Err pe
