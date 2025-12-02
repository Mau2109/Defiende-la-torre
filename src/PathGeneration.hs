module PathGeneration where

import Types
import Linear.V2 (V2(..))

-- =========================================================
-- FUNCIÓN PRINCIPAL PARA GENERAR CAMINO
-- =========================================================
-- Genera un camino procedural usando la curva del dragón
generatePath :: Int -> Path
generatePath level = generateSimplePath level
-- =========================================================
-- CURVA DEL DRAGÓN (FRACTAL RECURSIVO)
-- =========================================================
-- Generación de la curva del dragón (recursiva)
generateDragonCurve :: Int -> Path
generateDragonCurve 0 = [V2 0 0, V2 1 0]
generateDragonCurve n = 
    let prevCurve = generateDragonCurve (n-1)
        forward  = prevCurve
        backward = map rotate90CCW (reverse prevCurve)
    in forward ++ backward

-- Rotación de 90 grados en sentido contrario a las agujas del reloj
rotate90CCW :: V2 Double -> V2 Double
rotate90CCW (V2 x y) = V2 y (-x)

-- =========================================================
-- NORMALIZACIÓN DEL CAMINO
-- =========================================================
-- Normalizar el camino para que quepa en el área de juego
normalizePath :: Path -> Path
normalizePath path = 
    let (minX, maxX, minY, maxY) = boundingBox path
        width = maxX - minX
        height = maxY - minY
        scaleX = 700 / max 1 width
        scaleY = 450 / max 1 height
    in map (\(V2 x y) -> V2 (100 + x * scaleX) (50 + y * scaleY)) path

-- Calcular el bounding box del camino
boundingBox :: Path -> (Double, Double, Double, Double)
boundingBox path = 
    let xs = map (\(V2 x _) -> x) path
        ys = map (\(V2 _ y) -> y) path
    in (minimum xs, maximum xs, minimum ys, maximum ys)

-- =========================================================
-- ALTERNATIVAS DE GENERACIÓN DE CAMINOS
-- =========================================================

-- Camino simple con zigzag (útil para debugging)
generateSimplePath :: Int -> Path
generateSimplePath complexity =
  let segments = 3 + complexity
      width = 800
      height = 600
      
      -- Puntos intermedios con zigzag
      points = [ V2 (fromIntegral $ (i * width) `div` segments)
                    (fromIntegral $ if even i then 100 else height - 100)
               | i <- [0..segments]
               ]
  in points

-- Versión con curvas interpoladas
generatePathCurved :: Int -> Path
generatePathCurved complexity =
  let basePoints = generateSimplePath complexity
      -- Interpolar puntos para hacer el camino más suave
      interpolated = concatMap interpolate (zip basePoints (tail basePoints))
  in basePoints ++ interpolated
  where
    interpolate (V2 x1 y1, V2 x2 y2) =
      let midX = (x1 + x2) / 2
          midY = (y1 + y2) / 2
      in [V2 midX midY]

-- Función para generar camino con subdivisiones
generateSubdividedPath :: Int -> Path
generateSubdividedPath levels = normalizePath $ subdividePath levels [V2 0 0, V2 1 0]

subdividePath :: Int -> Path -> Path
subdividePath 0 path = path
subdividePath n path = subdividePath (n-1) (subdivideOnce path)

subdivideOnce :: Path -> Path
subdivideOnce path = concatMap subdivideSegment (zip path (tail path ++ [head path]))
  where
    subdivideSegment (p1, p2) = [p1, midpoint p1 p2, p2]

midpoint :: V2 Double -> V2 Double -> V2 Double
midpoint (V2 x1 y1) (V2 x2 y2) = V2 ((x1 + x2) / 2) ((y1 + y2) / 2)

-- =========================================================
-- FUNCIÓN WRAPPER PARA CAMINOS ALEATORIOS
-- =========================================================
-- Función para generar múltiples caminos y seleccionar uno
generateRandomPath :: Int -> Path
generateRandomPath complexity = generatePath complexity