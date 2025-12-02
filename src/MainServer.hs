module Main where

import Types
import PathGeneration
import NetworkServer
import Linear.V2 (V2(..))

main :: IO ()
main = do
  putStrLn "=== Iniciando servidor Tower Defense ==="
  
  -- Generar camino procedural
  let path = generatePath 5
  
  putStrLn $ "Camino generado con " ++ show (length path) ++ " puntos"
  
  -- Crear estado inicial del juego
  let initialState = GameState
        { gsPath = path
        , gsEnemies = []
        , gsTowers = []
        , gsProjectiles = []
        , gsWave = 1           -- Corregido: gsWave en lugar de gsWaveNumber
        , gsLives = 20
        , gsGold = 200         -- Corregido: gsGold en lugar de gsMoney
        , gsTime = 0
        , gsNextEnemyId = 0
        }
  
  -- Iniciar servidor en puerto 3000
  startServer 3000 initialState