module GraphCLI where

import Graphs

main :: IO ()
main = do
    putStrLn "\nCreando una nueva gráfica..."
    let g = crearGrafica
    gFinal <- menu g
    putStrLn "Gráfica final:"
    print gFinal

menu :: Grafica -> IO Grafica
menu g = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Agregar vértice"
    putStrLn "2. Agregar arista"
    putStrLn "3. Mostrar vértices"
    putStrLn "4. Mostrar aristas"
    putStrLn "5. Mostrar vecinos de un vértice"
    putStrLn "6. Mostrar grado de un vértice"
    putStrLn "7. Salir\n"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el vértice:"
            v <- readLn
            let g' = agregarVertice v g
            menu g'
        "2" -> do
            putStrLn "Ingrese el primer vértice de la arista:"
            v1 <- readLn
            putStrLn "Ingrese el segundo vértice de la arista:"
            v2 <- readLn
            let g' = agregarArista (v1, v2) g
            menu g'
        "3" -> do
            putStrLn "Vértices:"
            print $ vertices g
            menu g
        "4" -> do
            putStrLn "Aristas:"
            print $ aristas g
            menu g
        "5" -> do
            putStrLn "Ingrese el vértice:"
            v <- readLn
            putStrLn "Vecinos:"
            print $ vecinos v g
            menu g
        "6" -> do
            putStrLn "Ingrese el vértice:"
            v <- readLn
            putStrLn "Grado:"
            print $ grado v g
            menu g
        "7" -> return g
        _ -> do
            putStrLn "Opción no válida, intente de nuevo."
            menu g
