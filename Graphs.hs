module Graphs (
    Grafica,
    Vertice,
    Arista,
    crearGrafica,
    agregarVertice,
    agregarArista,
    vecinos,
    vertices,
    aristas,
    grado
) where

import Data.List (nub)

type Vertice = Int
type Arista = (Vertice, Vertice)

data Grafica = Grafica {
    vertices :: [Vertice],
    aristas :: [Arista]
} deriving (Show, Eq)


-- Crear una gráfica vacía
crearGrafica :: Grafica
crearGrafica = Grafica [] []


-- Agregar un vértice a la gráfica
agregarVertice :: Vertice -> Grafica -> Grafica
agregarVertice v (Grafica vs as) = Grafica (nub (v:vs)) as


-- Agregar una arista a la gráfica
agregarArista :: Arista -> Grafica -> Grafica
agregarArista (v1, v2) g@(Grafica vs as)
    | v1 `elem` vs && v2 `elem` vs = Grafica vs (nub ((v1, v2) : as))
    | otherwise = error "Ambos vértices deben existir en la gráfica para agregar una arista."


-- Obtener los vecinos de un vértice
-- Busca en ambas direcciones (v, w) y (w, v) para encontrar todos los vecinos
vecinos :: Vertice -> Grafica -> [Vertice]
vecinos vertice (Grafica _ aristas) = 
    -- Para cada arista (v1, v2), se verifica si v1 es igual al vertice dado. 
    -- Si la condición v1 == vertice es verdadera, v2 se agrega a la lista directNeighbors.
    let directNeighbors = [v2 | (v1, v2) <- aristas, v1 == vertice]
    -- Misma que el anterior, pero con el vertice contrario. 
        reverseNeighbors = [v1 | (v1, v2) <- aristas, v2 == vertice]
    in nub (directNeighbors ++ reverseNeighbors)


-- Calcular el grado de un vértice
grado :: Vertice -> Grafica -> Int
grado v grafica = length (vecinos v grafica)