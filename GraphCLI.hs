module GraphCLI where

import Data.Char
import Graphs
import System.Exit (exitSuccess)

-- Función para iniciar el programa con un menú principal
main :: IO ()
main = do
  putStrLn " \n==== Proyecto Final A: Haskell + MiniSAT ===="
  putStrLn " Proyecto realizado por los alumnos "
  putStrLn " - Paredes Zamudio Luis Daniel "
  putStrLn " - López García Luis Norberto  "
  putStrLn " - Eulogio Sánchez Christian\n"
  putStrLn " ==== Elige una opción: ==== "
  putStrLn " ==== 1. Resolver Ejercicios ===="
  putStrLn " ==== 2. Salir ===="
  putStrLn "Solo escribe el número, el punto no es necesario. \n"
  x <- getLine
  if not (all isDigit x)
    then
      putStrLn "No es una opción válida" >> main
    else case read x :: Int of
      1 -> initiate
      2 -> exitSuccess -- ? Buscar como salir del ghci desde este punto.
      _ -> putStrLn "No es una opción válida" >> main

-- Función principal para la interacción con el usuario
initiate :: IO ()
initiate = do
  putStrLn "\nCreando una nueva gráfica..."
  let g = crearGrafica
  gFinal <- menu g
  putStrLn "Gráfica final:"
  print gFinal

graphicSwitch :: Grafica -> IO Grafica
graphicSwitch g = do
  putStrLn "\nSelecciona la gráfica a usar"
  putStrLn "1. Gráfica de Petersen"
  putStrLn "2. Gráfica Mariposa"
  putStrLn "3. Gráfica K5"
  putStrLn "4. Gráfica 6-Ciclo"
  putStrLn "5. Regresar al Menu Anterior"
  opcion <- getLine
  case opcion of
    "1" -> do
      let g' = graficaPetersen
      return g'
    "2" -> do
      let g' = graficaMariposa
      return g'
    "3" -> do
      let g' = graficaK5
      return g'
    "4" -> do
      let g' = graficaCiclo6
      return g'
    "5" -> do
      putStrLn "Gráfica No Modificada. Regresando al Menu anterior..."
      return g
    _ -> do
      putStrLn "Gráfica No Modificada. Regresando al Menu anterior..."
      return g

-- Menú de opciones para interactuar con la gráfica
menu :: Grafica -> IO Grafica
menu g = do
  putStrLn "\nSeleccione una opción:"
  putStrLn "1. Seleccionar / Cambiar Gráfica"
  putStrLn "2. Creación Manual: Agregar Vértice"
  putStrLn "3. Creación Manual: Agregar Arista"
  putStrLn "4. Mostrar Vecinos de un Vértice"
  putStrLn "5. Mostrar Grado de un Vértice"
  putStrLn "6. Salir\n"
  opcion <- getLine
  case opcion of
    "1" -> do
      g' <- graphicSwitch g
      putStrLn("Gráfica actual: " ++ show g')
      print g'
      menu g'
    "2" -> do
      putStrLn "Ingrese el vértice:"
      v <- readLn
      let g' = agregarVertice v g
      putStrLn("Gráfica actualizada: " ++ show g')
      menu g'
    "3" -> do
      putStrLn "Ingrese el primer vértice de la arista:"
      v1 <- readLn
      putStrLn "Ingrese el segundo vértice de la arista:"
      v2 <- readLn
      let g' = agregarArista (v1, v2) g
      putStrLn("Gráfica actualizada: " ++ show g')
      menu g'
    "4" -> do
      putStrLn "Ingrese el vértice:"
      v <- readLn
      putStrLn "Vecinos: "
      print $ vecinos v g
      putStrLn("Gráfica actual: " ++ show g)
      menu g
    "5" -> do
      putStrLn "Ingrese el vértice:"
      v <- readLn
      putStrLn "Grado:"
      print $ grado v g
      putStrLn("Gráfica actual: " ++ show g)
      menu g
    "6" -> return g
    _ -> do
      putStrLn "Opción no válida, intente de nuevo."
      menu g
