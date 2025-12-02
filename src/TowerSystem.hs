module TowerSystem
  ( placeTower
  , updateTowers
  , towerCanShoot
  , towerShoot
  ) where

import Types
import Linear.V2 (V2(..))
import EnemyMovement (distance)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- ---------------------------------------------------------
--  COLOCACIÓN DE TORRES
-- ---------------------------------------------------------
placeTower :: TowerType -> Pos -> GameState -> GameState
placeTower tType pos gs =
  let cost = towerCost tType
  in if gsGold gs < cost
        then gs   -- No hay oro suficiente, no se coloca
        else gs
            { gsTowers = gsTowers gs ++ [createTower tType pos]
            , gsGold = gsGold gs - cost
            }

towerCost :: TowerType -> Int
towerCost Basic  = 100
towerCost Sniper = 150
towerCost Freeze = 120
towerCost Splash = 180

-- ---------------------------------------------------------
--  CREACIÓN DE UNA TORRE
-- ---------------------------------------------------------
createTower :: TowerType -> Pos -> Tower
createTower tType pos =
  case tType of
    Basic -> Tower pos Basic 1 0 130 20
    Sniper -> Tower pos Sniper 1 0 250 50
    Freeze -> Tower pos Freeze 1 0 100 5
    Splash -> Tower pos Splash 1 0 120 15

-- ---------------------------------------------------------
--  ACTUALIZACIÓN DE TODAS LAS TORRES
-- ---------------------------------------------------------
updateTowers :: GameState -> GameState
updateTowers gs =
  let (shots, updatedTowers) = unzip $ map (processTower (gsEnemies gs)) (gsTowers gs)
  in gs { gsTowers = updatedTowers
        , gsProjectiles = gsProjectiles gs ++ concat shots
        }

-- ---------------------------------------------------------
--  PROCESAR UNA TORRE INDIVIDUAL
-- ---------------------------------------------------------
processTower :: [Enemy] -> Tower -> ([Projectile], Tower)
processTower enemies tower
  | towerCooldown tower > 0 =
      ([], tower { towerCooldown = towerCooldown tower - 1 })
  | otherwise =
      case selectTarget tower enemies of
        Nothing -> ([], tower)
        Just target ->
          let proj = fireProjectile tower target
          in ([proj], tower { towerCooldown = towerBaseCooldown (towerType tower) })

towerBaseCooldown :: TowerType -> Int
towerBaseCooldown Basic  = 25
towerBaseCooldown Sniper = 45
towerBaseCooldown Freeze = 30
towerBaseCooldown Splash = 40

-- ---------------------------------------------------------
-- SELECCIÓN DE ENEMIGO (el más cercano al final)
-- ---------------------------------------------------------
selectTarget :: Tower -> [Enemy] -> Maybe Enemy
selectTarget tower enemies =
  let inRange = filter (\e -> distance (towerPos tower) (enemyPos e) <= towerRange tower) enemies
  in if null inRange
        then Nothing
        else Just $ minimumBy (comparing enemyPathIdx) inRange

-- ---------------------------------------------------------
--  DISPARO → CREAR PROYECTIL
-- ---------------------------------------------------------
fireProjectile :: Tower -> Enemy -> Projectile
fireProjectile tower enemy =
  Projectile
    { projPos = towerPos tower
    , projTarget = enemy
    , projDamage = towerDamage tower
    , projSpeed = projectileSpeedFor (towerType tower)
    }

projectileSpeedFor :: TowerType -> Double
projectileSpeedFor Basic  = 4
projectileSpeedFor Sniper = 7
projectileSpeedFor Freeze = 3
projectileSpeedFor Splash = 4

-- ---------------------------------------------------------
-- ¿LA TORRE PUEDE DISPARAR?
-- ---------------------------------------------------------
towerCanShoot :: Tower -> Bool
towerCanShoot t = towerCooldown t <= 0

towerShoot :: Tower -> Enemy -> Projectile
towerShoot = fireProjectile
