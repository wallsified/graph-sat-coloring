module GraphCLI where
import Data.Char
import System.Exit (exitSuccess)
import Graphs
import GraphColoring


-- Método para obtener unicamente ints al pedirle un valor al usuario en la CLI.
getInt :: IO Int
getInt = read <$> getLine

studentsInfoPrinter :: IO()
studentsInfoPrinter = do
  putStrLn " \n==== Proyecto Final A: Haskell + MiniSAT ===="
  putStrLn " Proyecto realizado por los alumnos "
  putStrLn " - Paredes Zamudio Luis Daniel "
  putStrLn " - López García Luis Norberto  "
  putStrLn " - Eulogio Sánchez Christian\n"
  putStrLn " ==== Elige una opción: ==== "
  putStrLn " ==== 1. Resolver Ejercicios ===="
  putStrLn " ==== 2. Salir ===="
  putStrLn "Solo escribe el número, el punto no es necesario. \n"

menuLoopInfoPrinter :: IO ()
menuLoopInfoPrinter  = do
  putStrLn "\nSeleccione una opción:"
  putStrLn "1. Seleccionar / Cambiar Gráfica"
  putStrLn "2. Creación Manual: Agregar Vértice"
  putStrLn "3. Creación Manual: Agregar Arista"
  putStrLn "4. Mostrar Vecinos de un Vértice"
  putStrLn "5. Mostrar Grado de un Vértice"
  putStrLn "6. Iniciar Coloración de la Gráfica Actual"
  putStrLn "7. Salir\n"

-- Se envia una gráfica vacía nueva para iniciar el loop de ejecución del menú
-- de la CLI. Luego se recibe de vuelta para cerrar el programa imprimiendo la 
-- gráfica en su estado final. 
graphStarterEnder :: IO ()
graphStarterEnder = do
  putStrLn "\nCreando una nueva gráfica..."
  let g = crearGrafica
  gFinal <- menuLoop g
  putStrLn "Gráfica final:"
  print gFinal

-- Método para, en el menu principal del programa, poder cambiar a alguna gráfica
-- popular, como la gráfica de Petersen, K5, etc. 
graphicSwitch :: Grafica -> IO Grafica
graphicSwitch g = do
  putStrLn "\nSelecciona la gráfica a usar"
  putStrLn "1. Gráfica de Petersen"
  putStrLn "2. Gráfica Mariposa"
  putStrLn "3. Gráfica K5"
  putStrLn "4. Gráfica 6-Ciclo"
  putStrLn "5. Grafica Aleatoria"
  putStrLn "6. Regresar al menu Anterior"
  opcion <- getInt
  case opcion of
    1 -> do
      let g' = graficaPetersen
      return g'
    2 -> do
      let g' = graficaMariposa
      return g'
    3 -> do
      let g' = graficaK5
      return g'
    4 -> do
      let g' = graficaCiclo6
      return g'
    5 -> do
      let g' = graficaRandom
      return g'
    6 -> do
      putStrLn "Gráfica No Modificada. Regresando al menu anterior..."
      return g
    _ -> do
      putStrLn "Gráfica No Modificada. Regresando al menu anterior..."
      return g

-- Método para iniciar el proceso de coloración de la gráfica en ejecución.
initiateColoration :: Grafica -> IO (Maybe [(Vertice, Color)], Grafica)
initiateColoration graph = do
  putStrLn "Ingrese el la cantidad de colores a verificar"
  kColors <- getInt
  let result = kColoracionSimple graph kColors
  case result of
        Just coloration -> print coloration
        Nothing -> putStrLn "No se pudo encontrar una coloración válida."
  return (result, graph)

-- Menú de opciones para interactuar con la gráfica
menuLoop :: Grafica -> IO Grafica
menuLoop g = do
  menuLoopInfoPrinter
  opcion <- getLine
  case opcion of
    "1" -> do
      g' <- graphicSwitch g
      putStrLn ("Gráfica actual: " ++ show g')
      menuLoop g'
    "2" -> do
      putStrLn "Ingrese el vértice:"
      v <- getInt
      let g' = agregarVertice v g
      putStrLn ("Gráfica actualizada: " ++ show g')
      menuLoop g'
    "3" -> do
      putStrLn "Ingrese el primer vértice de la arista:"
      v1 <- readLn
      putStrLn "Ingrese el segundo vértice de la arista:"
      v2 <- readLn
      let g' = agregarArista (v1, v2) g
      putStrLn ("Gráfica actualizada: " ++ show g')
      menuLoop g'
    "4" -> do
      putStrLn "Ingrese el vértice:"
      v <- getInt
      putStrLn "Vecinos: "
      print $ vecinos v g
      putStrLn ("Gráfica actual: " ++ show g)
      menuLoop g
    "5" -> do
      putStrLn "Ingrese el vértice:"
      v <- getInt
      putStrLn "Grado:"
      print $ grado v g
      putStrLn ("Gráfica actual: " ++ show g)
      menuLoop g
    "6" -> do
      (resultado, g) <- initiateColoration g
      menuLoop g
    "7" -> return g
    _ -> do
      putStrLn "Opción no válida, intente de nuevo."
      menuLoop g

-- Método principal de ejecución del programa.
main :: IO ()
main = do
  studentsInfoPrinter
  x <- getLine
  if not (all isDigit x)
    then
      putStrLn "No es una opción válida" >> main
    else case read x :: Int of
      1 -> graphStarterEnder
      2 -> exitSuccess -- ? Buscar como salir del ghci desde este punto.
      _ -> putStrLn "No es una opción válida" >> main
