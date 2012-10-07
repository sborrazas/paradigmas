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

-- 10
tokenizeStr :: [Char] -> String -> [String]
tokenizeStr _ [] = []
tokenizeStr lexico xs = if null principio then siguiente else if (length resto) <= 1 then [principio] else principio:siguiente
  where
    resto = dropWhile (\x -> not (any (==x) lexico)) xs
    principio = takeWhile (\x -> not (any (==x) lexico)) xs
    siguiente = tokenizeStr lexico (tail resto)

-- 11
parseNum :: Parse Char Token
parseNum = ((optional(token '-')) >*> (neList parseDig)) `build` charlistToExpr
  where
    isDigit :: Char -> Bool
    isDigit c = c >= '0' && c <= '9'
    parseDig :: Parse Char Char
    parseDig = spot isDigit
    charlistToExpr(x,xs) = Num (read (x++xs)::Int)

parseStr :: Parse Char Token
parseStr = ((token '"') >*> (spotWhile (\x -> x /= '"' )) >*> (token '"')) `build` convertidorStr
  where
    convertidorStr (c1, (str1, c2)) = Str str1

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
recognizeTokens xs = map (\x -> fst (last (parseToken x))) xs

-- 13
parseIntToken :: Parse Token Int
parseIntToken ((Num x):xs) = [(x, xs)]
parseIntToken x = []

parseStrToken :: Parse Token String
parseStrToken ((Str x):xs) = [(x, xs)]
parseStrToken x = []

parseMoveToken :: Parse Token Move
parseMoveToken ((Mv x):xs) = [(x, xs)]
parseMoveToken x = []

parseChrToken :: Parse Token Char
parseChrToken ((Chr x):xs) = [(x, xs)]
parseChrToken x = []

parseArrowToken :: Parse Token Token
parseArrowToken (Arrow:xs) = [(Arrow, xs)]
parseArrowToken x = []

parseEquivToken :: Parse Token Token
parseEquivToken (Equiv:xs) = [(Equiv, xs)]
parseEquivToken x = []

parseLSys :: Parse Token LSys
parseLSys = (
    (nTimes 10 parseIntToken) >*>
    parseStrToken >*>
    (list (parseChrToken >*> parseArrowToken >*> parseStrToken)) >*>
    (list (parseChrToken >*> parseEquivToken >*> parseMoveToken))
  ) `build` convertidorLsys
  where
    reglasF r = [(ch1, str1) | (ch1, (_, str1)) <- r]
    mapeosF m = [(ch1, mv1) | (ch1, (_, mv1)) <- m]
    toWord8 :: Int -> Word8
    toWord8 n = (read (show n)) :: Word8
    toFloat :: Int -> Float
    toFloat n = fromIntegral n
    convertidorLsys (numeros, (semilla, (reglas, mapeos))) = LSys
      (toFloat (numeros!!0)) -- Angle
      (toFloat (numeros!!1)) -- DeltaAngle
      (numeros!!2) -- PenWidth
      (numeros!!3) -- DeltaPenWidth
      (RGB (toWord8 (numeros!!4)) (toWord8 (numeros!!5)) (toWord8 (numeros!!6))) -- RGB
      ((numeros!!7), (numeros!!8), (numeros!!9)) -- DeltaRGB
      semilla -- Seed
      (reglasF reglas) -- Rules
      (mapeosF mapeos) -- Maps

--parseLSys :: Parse Token LSys
--parseLSys = ((nTimes 10 parseNum) >*> parseStr >*> (list (parseChr >*> parseArrow >*> parseStr)) >*> (list (parseChr >*> parseEquiv >*> parseMove))) `build` convertidorLsys
--  where
--    reglasF r = [(ch1, str1) | (Chr ch1, (_, Str str1)) <- r]
--    mapeosF m = [(ch1, mv1) | (Chr ch1, (_, Mv mv1)) <- m]
--    toWord8 :: Int -> Word8
--    toWord8 n = (read (show n)) :: Word8
--    toNum :: Token -> Int
--    toNum (Num n) = n
--    toFloat :: Token -> Float
--    toFloat (Num n) = fromIntegral n
--    convertidorLsys (numeros, (Str semilla, (reglas, mapeos))) = LSys (toFloat (numeros!!0)) (toFloat (numeros!!1)) (toNum (numeros!!2)) (toNum (numeros!!3)) (RGB (toWord8 (toNum (numeros!!4))) (toWord8 (toNum (numeros!!5))) (toWord8 (toNum (numeros!!6)))) ((toNum (numeros!!7)), (toNum (numeros!!8)), (toNum (numeros!!9))) semilla (reglasF reglas) (mapeosF mapeos)

-- 14
readLSys :: String -> LSys
readLSys contenido = (topLevel parseLSys) (recognizeTokens (tokenizeStr " \n" contenido))
