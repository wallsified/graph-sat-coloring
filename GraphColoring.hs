module GraphColoring
  ( Color,
    kColoracionSimple,
  )
where

import Graphs

-- Representamos los colores como enteros del 1 al k
type Color = Int
type Coloracion = Maybe [(Vertice, Color)]

-- kColoracion :: Grafica -> Int -> [Coloracion]

-- Función para encontrar una k-coloración única. Usando 'Maybe' declaramos
-- el hecho de que es posible no tener una k-coloración. Devuelve Just con una
-- lista de pares (vértice, color) si encuentra una coloración válida, o Nothing
-- si no es posible.
kColoracionSimple :: Grafica -> Int -> Coloracion
kColoracionSimple graph k = kColorar (obtenerVertices graph) (obtenerAristas graph) k []

-- Función que intenta colorear los vértices utilizando backtracking. Se recibe
-- una lista de vértices y una de aristas, el número de colores con los que se
-- busca colorear y la asignación actual de colores, por lo que es una función
-- recursiva.
--
-- Si no quedan vértices por colorear ([]), devuelve Just con la asignación
-- completa.
--
-- Funciona de la siguiente manera: Se intenta asignar un color válido al primer
-- vértice (v). Si hay asignaciones posibles (asignacionesPosibles), la función continúa con
-- el siguiente vértice. Si no hay ninguna asignación válida, devuelve Nothing.
kColorar :: [Vertice] -> [Arista] -> Int -> [(Vertice, Color)] -> Coloracion
kColorar [] _ _ asignacion = Just asignacion
kColorar (v : vs) as k asignacion =
  let coloresPosibles = [1 .. k]
      asignacionesPosibles = [(v, c) : asignacion | c <- coloresPosibles, esValido v c as asignacion]
   in case asignacionesPosibles of
        [] -> Nothing
        (a : _) -> kColorar vs as k a

-- Función que verifica si la asignación de color es válida
esValido :: Vertice -> Color -> [Arista] -> [(Vertice, Color)] -> Bool
esValido vertice color aristas asignacion =
  all
    ( \u -> case lookup u asignacion of
        Just cu -> cu /= color
        Nothing -> True
    )
    (adyacentes vertice aristas)

-- Función que devuelve los vértices adyacentes dado un vértice específico.
adyacentes :: Vertice -> [Arista] -> [Vertice]
adyacentes v as = [u | (u, v') <- as, v == v'] ++ [v' | (v', u) <- as, v == u]