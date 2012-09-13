module ParseLSys where

import Parser
import LSys
import Data.List
import Data.Word
import Graphics.HGL hiding (Angle)

data Token = Num Int | Str String | Mv Move | Chr Char | Arrow | Equiv
	deriving (Eq,Show)

parseToken :: Parse Char Token
parseToken = parseNum `alt` parseStr `alt` parseMove `alt` parseChr `alt` parseArrow `alt` parseEquiv

-- Funciones a definir

-- 10
tokenizeStr :: [Char] -> String -> [String]
tokenizeStr = undefined

-- 11
parseNum :: Parse Char Token
parseNum = undefined

parseStr :: Parse Char Token
parseStr = undefined

parseMove :: Parse Char Token
parseMove = undefined

parseChr :: Parse Char Token
parseChr = undefined

parseArrow :: Parse Char Token
parseArrow = undefined

parseEquiv :: Parse Char Token
parseEquiv = undefined

-- 12
recognizeTokens :: [String] -> [Token]
recognizeTokens = undefined

-- 13
parseLSys :: Parse Token LSys
parseLSys = undefined

-- 14
readLSys :: String -> LSys
readLSys = undefined



