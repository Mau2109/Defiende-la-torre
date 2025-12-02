module WaveSystem where

import Types
import EnemyMovement (addEnemy)
import Data.List (foldl')

-- Definimos una oleada como lista de tipos de enemigos
data Wave = Wave
  { waveNumber :: WaveNumber
  , waveEnemies :: [EnemyType]
  } deriving (Show)

-- Sistema básico de oleadas progresivas
generateWave :: WaveNumber -> Wave
generateWave n =
  let base = [Normal, Normal]
      extra =
        if n < 3 then [Fast]
        else if n < 5 then [Normal, Fast]
        else [Tank, Fast, Normal]
      scale = replicate (n `div` 2) Normal
  in Wave n (base ++ extra ++ scale)

-- Agregar los enemigos de una oleada al GameState
spawnWave :: Wave -> GameState -> GameState
spawnWave (Wave _ enemyTypes) gs =
  foldl' (\g eType -> addEnemy eType g) gs enemyTypes

-- Iniciar la siguiente oleada
startNextWave :: GameState -> GameState
startNextWave gs =
  let next = gsWave gs
      wave = generateWave next
      gs'  = spawnWave wave gs
  in gs' { gsWave = next + 1 }

-- ¿La oleada terminó?
waveFinished :: GameState -> Bool
waveFinished gs = null (gsEnemies gs)
