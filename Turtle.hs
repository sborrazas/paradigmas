module Turtle where

import Data.Word
import ICGraphics 
import Graphics.HGL hiding (Angle)
import LSys

type Vertex = (Float,Float)

type TurtleState = (Vertex,Angle,RGB,PenWidth)

type StackTurtleStates = [TurtleState]

type ColouredLine = (Vertex,Vertex,RGB,PenWidth) 

radians :: Float -> Float
radians a = a*(pi/180)

-- Operaciones con colores
sumColor :: RGB -> DeltaRGB -> RGB
sumColor (RGB r g b) (dr , dg , db) = RGB (aux r dr) (aux g dg) (aux b db)
    where aux :: Word8 -> Int -> Word8
          aux o d = if ((toInt o) + d >= 0 &&  (toInt o) + d <= 255) then (toWord8 ((toInt o) + d)) else o
          toInt n = (fromIntegral n)::Int
          toWord8 n = (fromIntegral n)::Word8

subtractColor :: RGB -> DeltaRGB -> RGB
subtractColor rgb (dr , dg , db) = sumColor rgb (-dr,-dg,-db)

-- Funciones a definir

-- 6
move :: Move -> TurtleState -> StackTurtleStates -> LSys -> (TurtleState , StackTurtleStates)
move  = undefined

-- 7
lsysMoves :: LSys -> Int -> [Move]
lsysMoves  = undefined

-- 8
turtleLines :: LSys -> TurtleState -> [Move] -> [ColouredLine]
turtleLines  = undefined

-- 9 
lsys :: LSys -> Int -> IO ()
lsys  = undefined

