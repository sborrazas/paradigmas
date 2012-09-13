module LSys where

import Data.Word
import Graphics.HGL hiding (Angle)

data Move = F | L | R | Pi | Pd | Ci | Cd | Bo | Bc | N
 deriving (Show,Read,Eq)

type Rule = (Char, String)  
type Rules = [Rule]  

type Map = (Char, Move)
type Maps = [Map]

type Angle = Float  -- angulo
type DeltaAngle = Float -- variacion del angulo

type PenWidth = Int -- grosor del lapiz
type DeltaPenWidth = Int -- variacion del grosor del lapiz

type DeltaRGB = (Int,Int,Int) -- variacion del color

instance Show RGB where
  show (RGB r g b) = "RGB " ++ show r ++ " " ++ show g ++ " " ++ show b

type Seed = String -- semilla

data LSys = LSys Angle DeltaAngle PenWidth DeltaPenWidth RGB DeltaRGB Seed Rules Maps
  deriving Show 

-- Funciones selectoras
angle :: LSys -> Angle
angle (LSys a _ _ _ _ _ _ _ _) = a

deltaAngle :: LSys -> DeltaAngle
deltaAngle (LSys _ d _ _ _ _ _ _ _) = d

penWidth :: LSys -> PenWidth
penWidth (LSys _ _ p _ _ _ _ _ _) = p

deltaPenWidth :: LSys -> DeltaPenWidth
deltaPenWidth (LSys _ _ _ w _ _ _ _ _) = w

color :: LSys -> RGB
color (LSys _ _ _ _ c _ _ _ _) = c

deltaColor :: LSys -> DeltaRGB
deltaColor (LSys _ _ _ _ _ d _ _ _) = d

seed :: LSys -> String
seed (LSys _ _ _ _ _ _ s _ _) = s

rules :: LSys -> Rules
rules (LSys _ _ _ _ _ _ _ r _) = r

maps :: LSys -> Maps
maps  (LSys _ _ _ _ _ _ _ _ m) = m

-- Funciones a definir
-- 1
lookupRule :: Rules -> Char -> String
lookupRule = undefined

-- 2
lookupMaps :: Maps -> Char -> Move
lookupMaps = undefined

-- 3
expandOne :: Rules -> String -> String
expandOne = undefined

-- 4
expandN :: Rules -> String -> Int -> String
expandN = undefined

-- 5
rewriteMaps :: Maps -> String -> [Move]
rewriteMaps = undefined


-- Ejemplos de L-Systems

arrowhead:: LSys
arrowhead = LSys 60 60 1 0 (RGB 255 255 255) (2,2,2) 
			"N" [('M',"N+M+N"),('N',"M-N-M")] [('M',F),('N',F),('+',L),('-',R)]

cross :: LSys
cross = LSys 90 90 1 0 (RGB 255 255 255) (-2, -2, -2) 
             "M-M-M-M" [('M',"M-M+M+MM-M-M+M")] [('M',F),('-',L),('+',R)]

dragon:: LSys
dragon = LSys 45.0 45.0 1 0 (RGB 255 255 255) (3,3,3) 
			  "MX" [('M',"A"),('X',"+MX--MY+"),('Y',"-MX++MY-")] 
			  [('M',F),('+',L),('-',R)]

mosaico:: LSys
mosaico = LSys 45 45 1 0 (RGB 255 255 255) (2,2,2) 
				"L--F--L--F" 
				[('L',"+R-F-R+"),('R',"-L+F+L-")] 
				[('F',F),('+',L),('-',R)]

snow:: LSys
snow = LSys 60 60 1 0 (RGB 255 255 255) (2,2,2) 
				 "M--M--M" [('M',"M+M--M+M")] [('M',F),('+',L),('-',R)]

				
plant:: LSys
plant = LSys 55.0 25.0 1 3 (RGB 200 0 0) (-10,30,-2) 
			 "X" [('X',"F-[[X]+X]+F[+FX]-X"),('F',"FF")] 
			 [('F',F),('+',R),('-',L),('[',Bo),(']',Bc)]

azulejo:: LSys
azulejo = LSys 90 36 1 0 (RGB 255 255 255) (2,2,2) 
				"6789" 
				[('6',"81++91----71[-81----61]++"),('7',"+81--91[---61--71]+"),('8',"-61++71[+++81++91]-"),('9',"--81++++61[+91++++71]--71")] 
				[('6',F),('7',F),('8',F),('9',F),('+',L),('-',R),('[',Bo),(']',Bc)]

tree :: LSys
tree = LSys 90 45 10 2 (RGB 160 90 45) (-10, 30, -2) 
            "M" [('M',"NPC[-M][+M][NM]"),('N',"NN")] 
            [('M',F),('N',F),('-',R),('+',L),('[',Bo),(']',Bc),('P',Pd),('C',Ci)]
			   
bush :: LSys
bush = LSys 90 22 10 2 (RGB 160 90 45) (-10, 20, -2) 
			"X" [('X',"M-PC[[X]+X]+M[+MX]-X"),('M',"MM")]
			[('M',F),('-',R),('+',L),('[',Bo),(']',Bc),('P',Pd),('C',Ci)]
			
