-- 	Library functions for parsing	
--                                                                       
--         (c) Simon Thompson, 1995,1998.					

-- Modificado levemente para el curso de Paradigmas de Programacion                                                                  

module Parser where
  
-- Tipo de los parsers						
type Parse a b = [a] -> [(b,[a])]
 
-- PARSERS BASICOS						
 
-- Falla siempre
none :: Parse a b
none inp = []
  
-- devuelve el valor dado sin importar el input 
succeed :: b -> Parse a b 
succeed val inp = [(val,inp)]
  
-- Reconoce un elemento t como primero del input
token :: Eq a => a -> Parse a a
token t (x:xs) 
  | t==x 	= [(t,xs)]
  | otherwise 	= []
token t []    = []
 
-- Reconoce un elemento que cumpla cierta condición como primero del input
spot :: (a -> Bool) -> Parse a a
spot p (x:xs) 
  | p x 	= [(x,xs)]
  | otherwise 	= []
spot p []    = []
  

-- COMBINADORES DE PARSERS
  
-- alt p1 p2 reconoce cualquier input reconocido por p1 o p2
alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp
  
-- Aplicación de un parser y luego otro al resultado del primero
infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp = [((y,z),r2) | (y,r1) <- p1 inp , (z,r2)  <- p2 r1 ]

-- Aplica una función al resultado de un parser 
build :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [ (f x,r) | (x,r) <- p inp ]
 
-- Reconoce cero o un objeto
optional :: Parse a b -> Parse a [b]
optional p = (succeed []) 
             `alt`  
             (p  `build` (:[]))

-- Reconoce una lista de objetos					
list :: Parse a b -> Parse a [b]
list p = (succeed []) 
         `alt`
         ((p >*> list p) `build` uncurry (:))

listAll            :: Parse a b -> Parse a [b]
listAll p inp      
    | null (p inp) = [([], inp)]
    | otherwise    = ((p >*> listAll p) `build` uncurry (:)) inp

-- Reconoce una lista no vacia de objetos
neList   :: Parse a b -> Parse a [b]
neList p = (p  `build` (:[]))
           `alt`
           ((p >*> neList p) `build` (uncurry (:)))
  
-- Reconoce un numero fijo de objetos
nTimes :: Int -> Parse a b -> Parse a [b]
nTimes 0 p     = succeed []
nTimes (n+1) p = (p >*> nTimes n p) `build` (uncurry (:))

-- Reconoce todos los que cumplen una condición 
spotWhile :: (a -> Bool) -> Parse a [a]
spotWhile p xs = [last(list (spot p) xs)]

-- TOP-LEVEL PARSER
topLevel :: Parse a b -> [a] -> b
topLevel p inp
  = case results of
      [] -> error "parse unsuccessful"
      _  -> head results
    where
    results = [ found | (found,[]) <- p inp ]













  
