module Turtle where

import Data.Word
import ICGraphics
import Graphics.HGL hiding (Angle)
import LSys
import List
import Data.Maybe

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
subtractColor rgb (dr, dg, db) = sumColor rgb (-dr, -dg, -db)

-- Helpers
rotateLeft :: TurtleState -> LSys -> TurtleState
-- rotateLeft _ _ = undefined
-- rotateLeft (position, angle, rgb, width) myLsys = (position, mod (angle + (deltaAngle myLsys)) 360, rgb, width)
rotateLeft (position, angle, rgb, width) myLsys = (position, angle + (deltaAngle myLsys), rgb, width)

rotateRight :: TurtleState -> LSys -> TurtleState
-- rotateRight _ _ = undefined
-- rotateRight (position, angle, rgb, width) myLsys = (position, mod (angle + 360 - deltaAngle myLsys) 360, rgb, width)
rotateRight (position, angle, rgb, width) myLsys = (position, angle + 360 - deltaAngle myLsys, rgb, width)

moveForward :: TurtleState -> LSys -> TurtleState
moveForward ((currentX, currentY), angle, rgb, width) _ = ((newX, newY), angle, rgb, width)
  where
    newX = currentX + cos (radians angle)
    newY = currentY + sin (radians angle)

increasePenWidth :: TurtleState -> LSys -> TurtleState
increasePenWidth (p, a, rgb, width) myLsys = (p, a, rgb, width + deltaPenWidth myLsys)

decreasePenWidth :: TurtleState -> LSys -> TurtleState
decreasePenWidth (p, a, rgb, width) myLsys = (p, a, rgb, if width >= dPenWidth then width - dPenWidth else width)
  where
    dPenWidth = deltaPenWidth myLsys

increaseColor :: TurtleState -> LSys -> TurtleState
increaseColor (p, a, rgb, width) myLsys = (p, a, sumColor rgb (deltaColor myLsys), width)

decreaseColor :: TurtleState -> LSys -> TurtleState
decreaseColor (p, a, rgb, width) myLsys = (p, a, subtractColor rgb (deltaColor myLsys), width)

moveExecuteMap :: [(Move, TurtleState -> LSys -> TurtleState)]
moveExecuteMap = [(F, moveForward), (L, rotateLeft), (R, rotateRight), (Pi, increasePenWidth), (Pd, decreasePenWidth), (Ci, increaseColor), (Cd, decreaseColor)]

getExecute :: Move -> (TurtleState -> LSys -> TurtleState)
getExecute m = snd (fromMaybe (undefined, moveForward) (find (\x -> fst x == m) moveExecuteMap))

-- Funciones a definir

-- 6
--  Recibe un movimiento, un estado de la tortuga, una pila de estados de la tortuga y un L-System.
--  Revuelve el estado resultante de ejecutar el movimiento dado a partir del estado dado y la nueva pila de estados de la tortuga.
--  Esta pila debe ser usada para ir guardando y restaurando los estados de la tortuga.
move :: Move -> TurtleState -> StackTurtleStates -> LSys -> (TurtleState, StackTurtleStates)
move N oldState stack _ = (oldState, stack)
move Bo oldState stack _ = (oldState, oldState:stack)
move Bc oldState (newState:remainingStack) _ = (newState, remainingStack)
move m oldState stack myLsys = (newState, stack)
  where
    newState = (getExecute m) oldState myLsys

-- 7
--  Recibe un LSys y un natural n, y devuelve la lista de movimientos resultante de aplicar n reescrituras a partir de la semilla
lsysMoves :: LSys -> Int -> [Move]
lsysMoves myLsys i = rewriteMaps (maps myLsys) (expandN (rules myLsys) (seed myLsys) i)

-- 8
--  Recibe un L-System, un estado de una tortuga, y una lista de movimientos.
--  Devuelve la lista de lineas resultante de aplicar los movimientos al estado dado para el L- System dado.
turtleLines :: LSys -> TurtleState -> [Move] -> [ColouredLine]
turtleLines myLsys initialState moves = filteredStates
  where
    getMovementStatesList :: [(Move, (TurtleState, StackTurtleStates))] -> [Move] -> [(Move, (TurtleState, StackTurtleStates))]
    getMovementStatesList oldStates [] = oldStates
    getMovementStatesList oldStates (movement:xs) = getMovementStatesList (oldStates ++ [(movement, move movement lastState lastStack myLsys)]) xs
      where
        (lastState, lastStack) = snd (last oldStates)
    states = getMovementStatesList [(F, (initialState, []))] (filter (/= N) moves)
    zippedStates = zip states (tail states)
    filteredStates = [(fromV, toV, fromC, fromW) | ((m1, ((fromV, _, fromC, fromW), fromStack)), (m2, ((toV, _, _, _), toStack))) <- zippedStates, m2 /= Bc]

-- 9
lsys :: LSys -> Int -> IO ()
lsys myLsys i = drawLines (turtleLines myLsys ((0, 0), (angle myLsys), (color myLsys), (penWidth myLsys)) (lsysMoves myLsys i))
