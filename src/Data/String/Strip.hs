module Data.String.Strip (strip,zero)  where

import Data.Char

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse


type Parser a = String -> [(a,String)]




result :: a -> Parser a
result v = \inp -> [(v,inp)]

zero :: Parser Char
zero = \inp -> []

item :: Parser Char
item = \inp -> case inp of
	[] -> []
	(x:xs) -> [(x,xs)]

seq :: Parser a -> Parser b -> Parser (a,b)
p `seq` q = \inp -> [((v,w),inp2) | (v,inp1) <- p inp
	, (w,inp2) <- q inp1]

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp1 | (v,inp1) <- p inp]

sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x ->
	if p x then result x else zero
	
char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')
lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')
upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')