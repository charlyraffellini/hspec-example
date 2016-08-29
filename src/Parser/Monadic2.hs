{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser.Monadic2 (Parser(Parser)
,parse
,runParser
,(>>=)
,return
,fmap
,pure
,(<*>)
,(<|>)
,empty
,oneOf
,satisfy
,chainl1
-- Implementations
,number
,reserved) where

--import Prelude hiding ((++))

import Data.Char
import Control.Monad
import Control.Applicative hiding (some,many)

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else (Parser (\cs -> []))

-- | One or more.
some :: Parser a -> Parser [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

-- | Zero or more.
many :: Parser a -> Parser [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

-- =============================
-- Implementations
-- =============================

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n




-- --type Parser a = String -> [(a,String)]
--
-- -- resultP :: a -> Parser a
-- -- resultP v = \inp -> [(v,inp)]
-- --
-- -- bindP :: Parser a -> (a -> Parser b) -> Parser b
-- -- p `bindP` f = \inp -> concat [f v inp1 | (v,inp1) <- p inp]
--
--
-- data Parser a = Value {parser:: (String -> [(a,String)])}
--
-- -- instance Functor Parser where
-- -- 	fmap f (Value a) = Value (f a)
-- --
-- --
-- -- instance Applicative Parser where
-- --     pure v = Value (\inp -> [(v,inp)])
-- --     --Fail <*> _ = Fail
-- --     (Value f) <*> something = fmap f something
--
--
-- instance Monad Parser where
--     return v = Value (\inp -> [(v,inp)])
--     (Value p) >>= f = Value (\inp -> concat [parser (f) v inp1 | (v,inp1) <- p inp])
--
--
--
-- -- class Monad m where
-- -- 	result :: a -> m a
-- -- 	bind :: m a -> (a -> m b) -> m b
--
-- -- instance Monad Parser where
-- -- 	--result :: a -> Parser a
-- -- 	result = resultP
-- -- 	--bind :: Parser a -> (a -> Parser b) -> Parser b
-- -- 	p `bind` f = bindP
--
--
-- -- class Monad m => Monad0Plus m where
-- -- 	zero :: m a
-- -- 	(++) :: m a -> m a -> m a
-- -- --
-- -- --
-- -- instance Monad0Plus (Parser) where
-- -- 	zero :: Parser a
-- -- 	zero = \inp -> []
-- -- 	(++) :: Parser a -> Parser a -> Parser a
-- -- 	p ++ q = \inp -> (p inp ++ q inp)
-- -- --
-- -- --
-- -- string :: String -> Parser String
-- -- string "" = [""]
-- -- string (x:xs) = [x:xs | _ <- char x, _ <- string xs]
