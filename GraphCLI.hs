module GraphCLI where

import Data.Char
import GraphColoring
import Graphs
import System.Exit (exitSuccess)

-- Método para obtener unicamente ints al pedirle un valor al usuario
-- en la CLI.
getInt :: IO Int
getInt = read <$> getLine

-- Función para imprimir la información de los estudiantes. También sirve
-- como la primera pantalla del menu de la CLI.
studentsInfoPrinter :: IO ()
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

-- Función para imprimir las diversas opciones del menu principal
-- de la CLI.
menuLoopInfoPrinter :: IO ()
menuLoopInfoPrinter = do
  putStrLn "\nSeleccione una opción:"
  putStrLn "1. Seleccionar / Cambiar Gráfica"
  putStrLn "2. Creación Manual: Agregar Vértice"
  putStrLn "3. Creación Manual: Agregar Arista"
  --putStrLn "4. Mostrar Vecinos de un Vértice"
  --putStrLn "5. Mostrar Grado de un Vértice"
  putStrLn "4. Iniciar Coloración de la Gráfica Actual"
  putStrLn "5. Salir\n"

-- Se envia una gráfica vacía nueva para iniciar el loop de ejecución del menú
-- de la CLI. Luego se recibe de vuelta para cerrar el programa imprimiendo la
-- gráfica en su estado final.
graphStarterEnder :: IO ()
graphStarterEnder = do
  putStrLn "\nCreando una nueva gráfica..."
  let g = crearGrafica
  gFinal <- menuLoop g
  putStrLn "\nGráfica final:"
  print gFinal

-- Método para, en el menu principal del programa, poder cambiar a alguna gráfica
-- popular, como la gráfica de Petersen, K5, etc. Las gráficas a las que se puede
-- cambiar se muestran en el archivo 'Graphs.hs'
graphicSwitch :: Grafica -> IO Grafica
graphicSwitch g = do
  putStrLn "\nSelecciona la gráfica a usar. Puedes verlas en imagen"
  putStrLn "en la carpeta 'imagenes' en la carpeta raiz del programa."
  putStrLn "1. Gráfica de Petersen"
  putStrLn "2. Gráfica Moño"
  putStrLn "3. Gráfica K5"
  putStrLn "4. Gráfica 6-Ciclo"
  putStrLn "5. Grafica de Prueba"
  putStrLn "6. Regresar al menu Anterior"
  opcion <- getInt
  case opcion of
    1 -> do
      let nuevaGrafica = graficaPetersen
      return nuevaGrafica
    2 -> do
      let nuevaGrafica = graficaMoño
      return nuevaGrafica
    3 -> do
      let nuevaGrafica = graficaK5
      return nuevaGrafica
    4 -> do
      let nuevaGrafica = graficaCiclo6
      return nuevaGrafica
    5 -> do
      let nuevaGrafica = graficaPrueba
      return nuevaGrafica
    6 -> do
      putStrLn "\nGráfica No Modificada. Regresando al menu anterior..."
      return g
    _ -> do
      putStrLn "\nGráfica No Modificada. Regresando al menu anterior..."
      return g

-- Método para iniciar el proceso de coloración de la gráfica en ejecución.
initiateColoration :: Grafica -> IO (Maybe [Coloracion], Grafica)
initiateColoration graph = do
  putStrLn "'\nIngrese el la cantidad de colores a verificar"
  kColors <- getInt
  coloraciones <- kColoracion graph kColors
  if null coloraciones
    then putStrLn "\nNo se pudo encontrar una coloración válida."
    else do
      putStrLn "\nEl Patron resultante es el siguiente: en la primera entrada"
      putStrLn "se encuentra el número del vértice y después el color asignado a este.\n"
      mapM_ print coloraciones
  return (if null coloraciones then Nothing else Just coloraciones, graph)

-- Menú de opciones para interactuar con la gráfica
menuLoop :: Grafica -> IO Grafica
menuLoop grafica = do
  menuLoopInfoPrinter
  opcion <- getLine
  case opcion of
    "1" -> do
      nuevaGrafica <- graphicSwitch grafica
      putStrLn ("\nGráfica actual: " ++ show nuevaGrafica)
      menuLoop nuevaGrafica
    "2" -> do
      putStrLn "Ingrese el vértice:"
      v <- getInt
      let nuevaGrafica = agregarVertice v grafica
      putStrLn ("\nGráfica actualizada: " ++ show nuevaGrafica)
      menuLoop nuevaGrafica
    "3" -> do
      putStrLn "\nIngrese el primer vértice de la arista:"
      v1 <- readLn
      putStrLn "\nIngrese el segundo vértice de la arista:"
      v2 <- readLn
      let nuevaGrafica = agregarArista (v1, v2) grafica
      putStrLn ("\nGráfica actualizada: " ++ show nuevaGrafica)
      menuLoop nuevaGrafica
{-     "4" -> do
      putStrLn "\nIngrese el vértice:"
      v <- getInt
      putStrLn "\nVecinos: "
      print $ vecinos v grafica
      putStrLn ("\nGráfica actual: " ++ show grafica)
      menuLoop grafica -}
{-     "5" -> do
      putStrLn "Ingrese el vértice:"
      v <- getInt
      putStrLn "Grado:"
      print $ grado v grafica
      putStrLn ("\nGráfica actual: " ++ show grafica)
      menuLoop grafica -}
    "4" -> do
      (resultado, grafica) <- initiateColoration grafica
      putStrLn ("\nGráfica actual: " ++ show grafica)
      menuLoop grafica
    "5" -> return grafica
    _ -> do
      putStrLn "\nOpción no válida, intente de nuevo."
      menuLoop grafica

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
      2 -> exitSuccess
      _ -> putStrLn "No es una opción válida" >> main
