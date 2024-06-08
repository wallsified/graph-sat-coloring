module Graphs
  ( Grafica,
    Vertice,
    Arista,
    crearGrafica,
    agregarVertice,
    agregarArista,
    vertices,
    aristas,
    graficaCiclo6,
    graficaK5,
    graficaMoño,
    graficaPetersen,
    graficaPrueba,
    obtenerAristas,
    obtenerVertices,
  )
where

import Data.List (nub)

-- Representamos los vértices como enteros del 1 al n
type Vertice = Int

-- Representamos las aristas como una tupla de vértices izquierdo
-- y derecho.
type Arista = (Vertice, Vertice)

-- Representamos una Gráfica como un conjunto conformado
-- por una lista de vértices y una lista de aristas.
data Grafica = Grafica
  { vertices :: [Vertice],
    aristas :: [Arista]
  }
  deriving (Show, Eq)

-- Crea una gráfica vacía
crearGrafica :: Grafica
crearGrafica = Grafica [] []

-- Agrega un vértice a la gráfica. Los vértices son identificados
-- por un número, por lo que se agregan usando Ints. Usamos
-- 'nub' para quitar los duplicados que se pudieran encontrar.
agregarVertice :: Vertice -> Grafica -> Grafica
agregarVertice v (Grafica vs as) = Grafica (nub (v : vs)) as

-- Agrega una arista a la gráfica. Ambos vértices deben existir en
-- la gráfica para agregar una arista.
agregarArista :: Arista -> Grafica -> Grafica
agregarArista (v1, v2) g@(Grafica vs as)
  | v1 `elem` vs && v2 `elem` vs = Grafica vs (nub ((v1, v2) : as))
  | otherwise = error "Ambos vértices deben existir en la gráfica para agregar una arista."

-- Obtención de los vértices de una gráfica.
obtenerVertices :: Grafica -> [Vertice]
obtenerVertices (Grafica vs _) = vs

-- Obtención de los vértices de una gráfica.
obtenerAristas :: Grafica -> [Arista]
obtenerAristas (Grafica _ as) = as

-- Gráfica de Petersen
graficaPetersen :: Grafica
graficaPetersen = Grafica [1 .. 10] [(1, 6), (1, 5), (1, 2), (2, 7), (2, 3), (3, 8), (3, 4), (4, 9), (4, 5), (5, 10), (6, 8), (6, 9), (7, 9), (7, 10), (8, 10)]

-- Gráfica "Moño"
graficaMoño :: Grafica
graficaMoño = Grafica [1 .. 5] [(1, 2), (1, 3), (2, 3), (3, 4), (3, 5), (4, 5)]

-- Gráfica K5, Completa-5
graficaK5 :: Grafica
graficaK5 = Grafica [1 .. 5] [(1, 2), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5), (3, 4), (3, 5), (4, 5)]

-- Gráfica 6-Ciclo, un hexágono.
graficaCiclo6 :: Grafica
graficaCiclo6 = Grafica [1 .. 6] [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 1)]

-- Gráfica de Prueba
graficaPrueba :: Grafica
graficaPrueba = Grafica [1 .. 4] [(1, 2), (2, 3), (3, 4), (4, 1), (1, 3)]