module Main where

import ParseLSys
import Parser
import LSys
import Turtle

main = do 
  putStrLn "Ingrese el nombre del archivo:"
  name <- getLine
  x <- readFile name  
  putStrLn "Ingrese las iteraciones:"
  n <- getLine
  let ls = readLSys x
      it = ((read n)::Int)
  putStrLn (show ls)
  lsys ls it
          

