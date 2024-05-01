module Main where

import SAT.MiniSat (solve) -- import temporal
import Data.Char





main :: IO()
main = do
    putStrLn " ==== Proyecto Final A: Haskell + MiniSAT ===="
    putStrLn " Proyecto realizado por los alumnos "
    putStrLn " - Paredes Zamudio Luis Daniel "
    putStrLn " - López García Luis Norberto  "
    putStrLn " - Eulogio Sánchez Christian"
    putStrLn " ==== Elige una opción: ==== "
    putStrLn " ==== 1. Resolver Ejercicios ===="
    putStrLn " ==== 2. Salir ===="
    x <- getLine
    if not (all isDigit x) then
        putStrLn "No es una opción válida" >> main
    else
        case read x :: Int of
      -- 1 -> readSolve >> main -- Aqui se ejecuta algo a posteriori
      2 -> return ()
      _ -> putStrLn "No es una opción válida" >> main
