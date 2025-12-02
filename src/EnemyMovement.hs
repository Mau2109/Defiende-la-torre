module EnemyMovement
  ( spawnEnemy
  , enemyStats
  , moveEnemy
  , moveEnemyAlongPath
  , advanceToTarget
  , hasReachedTarget
  , addEnemy
  , stepGame
  , enemyProgress
  , distance       -- <<=== IMPORTANTE
  ) where

import Types
import Linear.V2 (V2(..))
import Data.List (partition)

-- ============================
-- FUNCIONES VECTORIALES
-- ============================

vectorSubtract :: V2 Double -> V2 Double -> V2 Double
vectorSubtract (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

vectorAdd :: V2 Double -> V2 Double -> V2 Double
vectorAdd (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

vectorLength :: V2 Double -> Double
vectorLength (V2 x y) = sqrt (x*x + y*y)

scaleVector :: Double -> V2 Double -> V2 Double
scaleVector s (V2 x y) = V2 (s*x) (s*y)

-- ============================
-- FUNCION DISTANCE
-- ============================

distance :: Pos -> Pos -> Double
distance (V2 x1 y1) (V2 x2 y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- ============================
-- CREACION DE ENEMIGOS
-- ============================

spawnEnemy :: EnemyType -> Int -> Path -> Enemy
spawnEnemy enemyType nextId path = Enemy
  { enemyId      = nextId
  , enemyType    = enemyType
  , enemyPos     = head path
  , enemyPathIdx = 0
  , enemyHP      = maxHP
  , enemyMaxHP   = maxHP
  , enemySpeed   = speed
  , enemyReward  = reward
  }
  where
    (maxHP, speed, reward) = enemyStats enemyType

enemyStats :: EnemyType -> (Health, Double, Gold)
enemyStats Normal  = (100, 50, 10)
enemyStats Fast    = (50, 100, 8)
enemyStats Tank    = (300, 25, 20)
enemyStats Flying  = (80, 75, 12)

-- ============================
-- MOVIMIENTO DE ENEMIGOS
-- ============================

moveEnemy :: Double -> Path -> Enemy -> Enemy
moveEnemy dt path enemy
  | enemyPathIdx enemy >= length path - 1 = enemy
  | otherwise =
      let nextIdx   = enemyPathIdx enemy + 1
          targetPos = path !! nextIdx
          moved     = advanceToTarget dt targetPos enemy
      in if hasReachedTarget moved targetPos
         then moved { enemyPathIdx = nextIdx }
         else moved

moveEnemyAlongPath :: Double -> Path -> Enemy -> Enemy
moveEnemyAlongPath = moveEnemy

advanceToTarget :: Double -> Pos -> Enemy -> Enemy
advanceToTarget dt targetPos enemy =
  let currentPos   = enemyPos enemy
      diffVec      = vectorSubtract targetPos currentPos
      dist         = vectorLength diffVec
      speed        = enemySpeed enemy
      moveDistance = min dist (speed * dt)
  in if dist > 0
      then
        let direction = scaleVector (moveDistance / dist) diffVec
            newPos    = vectorAdd currentPos direction
        in enemy { enemyPos = newPos }
      else enemy

hasReachedTarget :: Enemy -> Pos -> Bool
hasReachedTarget enemy targetPos =
  vectorLength (vectorSubtract (enemyPos enemy) targetPos) < 5.0

-- ============================
-- GAME STATE UPDATE
-- ============================

addEnemy :: EnemyType -> GameState -> GameState
addEnemy eType state =
  let nextId   = length (gsEnemies state)
      newEnemy = spawnEnemy eType nextId (gsPath state)
  in state { gsEnemies = gsEnemies state ++ [newEnemy] }

stepGame :: Double -> GameState -> GameState
stepGame dt state =
  let moved               = map (moveEnemy dt (gsPath state)) (gsEnemies state)
      (finished, alive)   = partition (\e -> enemyPathIdx e >= length (gsPath state) - 1) moved
      newLives            = gsLives state - length finished
  in state { gsEnemies = alive, gsLives = newLives }

enemyProgress :: Enemy -> Path -> Double
enemyProgress enemy path =
  fromIntegral (enemyPathIdx enemy)
  / fromIntegral (length path - 1)
