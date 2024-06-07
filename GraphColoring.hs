module GraphColoring
  ( kColoracion,
    Coloracion,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Graphs
import SAT.MiniSat

-- Representamos los colores como enteros del 1 al k
type Color = Int

-- Definimos las variables de coloración
data ColorFormula = ColorFormula Vertice Color deriving (Eq, Ord, Show)

-- El tipo Coloración será la lista de tuplas vértice-color asignado
type Coloracion = [(Vertice, Color)]

-- Crear variables de color para cada vértice y color. Iteramos sobre
-- cada vértice y sobre cada número del 1 al k para regresar una lista
-- con las formulas de Color.
colorFormulas :: [Vertice] -> Int -> [ColorFormula]
colorFormulas vs k = [ColorFormula v c | v <- vs, c <- [1 .. k]]

-- Crear fórmula booleana que representa el problema de k-coloración.
-- La fórmula asegura que cada vértice tenga exactamente un color y
-- que vértices adyacentes no compartan el mismo color.
--
-- Al final se combinan ambas condiciones en una fórmula booleana
-- utilizando 'All', que exige que ambas listas de cláusulas sean satisfechas.
kColorFormula :: Grafica -> Int -> Formula ColorFormula
kColorFormula g k = All (unSoloColorPorVertice ++ adjacenciaSinMismoColor)
  where
    vs = obtenerVertices g
    as = obtenerAristas g

    -- Para cada vértice v, se genera una cláusula que exige que exactamente
    -- una de las variables ColorFormula v c sea verdadera (es decir, el
    -- vértice v tiene un solo color c).
    unSoloColorPorVertice = [ExactlyOne [Var (ColorFormula v c) | c <- [1 .. k]] | v <- vs]

    -- Para cada par de vértices adyacentes (v, u), genera una cláusula que
    -- prohíbe que v y u tengan el mismo color c.
    adjacenciaSinMismoColor = [All [Not (Var (ColorFormula v c) :&&: Var (ColorFormula u c)) | c <- [1 .. k]] | (v, u) <- as]

-- Se verifica si una coloración es una k-coloración válida. Primero
-- se extraen los colores usados en la coloración (la segunda parte
-- de las tuplas) y luego se verifica que cada color del 1 al k
-- esté presente en la lista de colores usados.
--
-- Esto evita situaciones donde, si se tiene una k-coloracion, pero
-- no una n-coloracion, con n>k, se reemplaze dicho color para que
-- se genere una coloración "válida".
esColoracionValida :: Int -> Coloracion -> Bool
esColoracionValida k coloracion = all (`elem` colors) [1 .. k]
  where
    colors = map snd coloracion

-- Verificación completa de la k-coloración
kColoracion :: Grafica -> Int -> IO [Coloracion]
kColoracion g k = do
  -- Se crean todas las fórmulas de Color.
  let formula = kColorFormula g k

  -- Usando MiniSAT, buscamos todas las soluciones que satisfagan
  -- el problema (pero aun no se validan si son coloraciones
  -- válidas).
  let soluciones = solve_all formula

  -- Se convierten las soluciones del solucionador en coloraciones,
  -- filtrando las variables verdaderas y transformándolas en una
  -- lista de tuplas vértice-color.
  let coloraciones = map (map (\(ColorFormula v c, _) -> (v, c)) . Map.toList . Map.filter id) soluciones

  -- Se filtran las coloraciones para asegurar que solo las válidas
  -- (que usan todos los colores del 1 al k) sean incluidas.
  let coloracionesValidas = filter (esColoracionValida k) coloraciones

  return coloracionesValidas -- Se devuelven las coloraciones válidas.
