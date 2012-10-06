module ParseLSys where

import Parser
import LSys
import Data.List
import Data.Word
import Graphics.HGL hiding (Angle)
import Data.Maybe

data Token = Num Int | Str String | Mv Move | Chr Char | Arrow | Equiv
	deriving (Eq,Show)

parseToken :: Parse Char Token
parseToken = parseNum `alt` parseStr `alt` parseMove `alt` parseChr `alt` parseArrow `alt` parseEquiv

-- Funciones a definir

--parserSeparadores s = filter (\(x, y) -> not (null x) ) demo
--  where
--    demo = (list (spotWhile (\x -> x /= ' ' && x /= '\n') >*> (token ' ' `alt` token '\n') )) s

-- 10
tokenizeStr :: [Char] -> String -> [String]
tokenizeStr _ [] = []
tokenizeStr lexico xs = if null principio then siguiente else if (length resto) <= 1 then [principio] else principio:siguiente
  where
    resto = dropWhile (\x -> not (any (==x) lexico)) xs
    principio = takeWhile (\x -> not (any (==x) lexico)) xs
    siguiente = tokenizeStr lexico (tail resto)

--tokenizeStr lexico [] = []
--tokenizeStr lexico (x:xs) = if any (==x) lexico then nextTok ++ [""] else nextTok ++ [x:(last nextTok :: String)]
--  where
--    nextTok = tokenizeStr lexico xs


-- 11
parseNum :: Parse Char Token
parseNum = (neList parseDig) `build` charlistToExpr
  where
    isDigit :: Char -> Bool
    isDigit c = c >= '0' && c <= '9'
    parseDig :: Parse Char Char
    parseDig = spot isDigit
    charlistToExpr xs = Num (read xs :: Int)

parseStr :: Parse Char Token
parseStr = ((token '"') >*> (spotWhile (\x -> x /= '"' )) >*> (token '"')) `build` convertidorStr
  where
    convertidorStr (c1, (str1, c2)) = Str str1

--F | L | R | Pi | Pd | Ci | Cd | Bo | Bc | N
parseMove :: Parse Char Token
parseMove = (((token 'F') `build` convertidorMv1)
  `alt` ((token 'L') `build` convertidorMv1)
  `alt` ((token 'R') `build` convertidorMv1)
  `alt` ((token 'N') `build` convertidorMv1)
  `alt` ((token 'P' >*> token 'i') `build` convertidorMv2)
  `alt` ((token 'P' >*> token 'd') `build` convertidorMv2)
  `alt` ((token 'C' >*> token 'i') `build` convertidorMv2)
  `alt` ((token 'C' >*> token 'd') `build` convertidorMv2)
  `alt` ((token 'B' >*> token 'o') `build` convertidorMv2)
  `alt` ((token 'B' >*> token 'c') `build` convertidorMv2))
  `build` convertidorMvFinal
  where
    moveExecuteMap = [(F, "F"), (L, "L"), (R, "R"), (Pi, "Pi"), (Pd, "Pd"), (Ci, "Ci"), (Cd, "Cd"), (Bc, "Bc"), (Bo, "Bo")]
    convertidorMv1 c = [c]
    convertidorMv2 (c1, c2) = [c1, c2]
    convertidorMvFinal m = Mv (fst (fromMaybe (N, "") (find (\x -> snd x == m) moveExecuteMap)))

parseChr :: Parse Char Token
parseChr = ((token '\'') >*> (spot (\x -> x/= '\'')) >*> (token '\'')) `build` convertidorChar
  where
    convertidorChar (_, (c2, _)) = Chr c2

parseArrow :: Parse Char Token
parseArrow = (token '-' >*> token '>') `build` (\x -> Arrow)

parseEquiv :: Parse Char Token
parseEquiv = token '~' `build` (\x -> Equiv)

-- 12
recognizeTokens :: [String] -> [Token]
recognizeTokens xs = map (\x -> fst ( head (parserMap x)) ) xs
  where parserMap = parseNum `alt` parseStr `alt` parseMove `alt` parseChr `alt` parseArrow `alt` parseEquiv

-- 13
--([Token],(Token,([(Token,(Token,Token))], ([(Token,Token)],Token)))), [Char]
--Lsys>>  Angle DeltaAngle PenWidth DeltaPenWidth RGB DeltaRGB Seed Rules Maps
--type Rule = (Char, String)
--type Rules = [Rule]
--type Map = (Char, Move)
--
parseLSys :: Parse Token LSys
parseLSys = (nTimes 10 parseNum) >*>
  parseStr >*>
  (list (parseChr >*> parseArrow >*> parseStr)) >*>
  (list (parseChr >*> parseEquiv) >*> parseMove)
  where
    reglasF r = [(ch1, str1) | (Chr ch1, (_, Str str1)) <- r]
    mapeosF m = [(ch1, mv1) | (Chr ch1, (_, Mv mv1)) <- m]
    toWord8 :: Token -> Word8
    toWord8 (Num n) = toEnum (fromInteger (toInteger n)) :: Word8
    toNum :: Token -> Int
    toNum (Num n) = n
    convertidorLsys (numeros, (Str semilla, (reglas, (mapeos)))) = LSys (toNum numeros!!0)
      (toNum (numeros!!1)) (toNum (numeros!!2)) (toNum (numeros!!3)) (RGB (toWord8 (numeros!!4)) (toWord8 (numeros!!5)) (toWord8 (numeros!!6))) ((toNum (numeros!!7)), (toNum (numeros!!8)), (toNum (numeros!!9))) semilla (reglasF reglas) (mapeosF mapeos)


-- 14
readLSys :: String -> LSys
readLSys = undefined
