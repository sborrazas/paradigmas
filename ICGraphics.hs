module ICGraphics where

import Array
import Graphics.HGL
-- Data 
type PL = ( ( Float, Float ), ( Float, Float ), RGB, Int )

-- Constants
borderSize :: Int
borderSize = 50

imageSize :: Int
imageSize = 500

windowSize :: Int
windowSize = imageSize + borderSize 

--
-- drawLines - 
--
drawLines :: [ PL ] -> IO()
drawLines ls
  = drawLines' ls p s
  where
    ( p, s ) = computeLineScale ls

drawLines' ls ( minx, miny ) width
  = runGraphics ( do
      w <- openWindow "Laboratorio Fractales" ( windowSize, windowSize )
--      drawInWindow w $ text (450, 490) "Paradigmas"
      lines w ls
      getKey w
      closeWindow w
      )
  where
    lines w []
      = return()
    lines w ( ( p, p', c, s ) : ls )
      = do drawInWindow w ( colLine ( scale p ) ( scale p' ) c s )
           lines w ls
    colLine x y c s
      = pen ( \p -> withPen p ( line x y ) )
      where
        pen = mkPen Solid s c --( colorTable ! c )
    scale:: (Float, Float) -> (Int, Int )
    scale ( x, y ) = ( b2 + round ( ( x - minx ) * r ), 
                       windowSize - ( b2 + round ( ( y - miny ) * r ) ) )
    r = fromIntegral imageSize / width
    b2 = borderSize `div` 2

--
-- computeScale finds the 'lower left' corner and width of the axis-aligned
-- bounding box in 2D space. It caters for disconnected regions. 
-- Note that if you walk the list of lines all at once you get stack overflow
-- for quite modest lists. This version breaks the lists into sublists of
-- length 100 and patches up afterwards - a real mess, but a problem with the
-- implementation of foldl. 
--
computeLineScale ls = computeScale' ( concat ( map f ls ) )
  where f ( p, p', c, s ) = [ p, p' ]

computeScale' ls 
  = ( ( minx, miny ), max ( maxx - minx ) ( maxy - miny ) )
  where
    ( minx, maxx, miny, maxy ) = 
       ( minimum ( map first xs ), maximum ( map second xs ),
         minimum ( map third xs ), maximum ( map fourth xs ) )

    xs = map ( foldr min4 ( inf, -inf, inf, -inf ) ) ( split ls 100 )

    inf :: Float
    inf = 10000000
 
    min4 ( x, y ) ( a, b, c, d )
      = ( min a x, max b x, min c y, max d y ) 

    first ( a, b, c, d )  = a
    second ( a, b, c, d ) = b
    third ( a, b, c, d )  = c
    fourth ( a, b, c, d ) = d

    split :: [ a ] -> Int -> [ [ a ] ]
    split [] n
      = []
    split xs n
      = a : split b n
      where ( a, b ) = splitAt n xs
