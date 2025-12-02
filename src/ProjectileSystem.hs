module ProjectileSystem 
  ( updateProjectiles
  ) where

import Types
import Linear.V2 (V2(..))
import EnemyMovement (distance)
import Data.List (partition)

-- ---------------------------------------------------------
-- 🔄 ACTUALIZACIÓN GENERAL DE PROYECTILES
-- ---------------------------------------------------------
updateProjectiles :: GameState -> GameState
updateProjectiles gs =
  let (remainingProjectiles, damagedEnemies) = processProjectiles (gsProjectiles gs) (gsEnemies gs)
  in gs { gsProjectiles = remainingProjectiles
        , gsEnemies = damagedEnemies
        }

-- ---------------------------------------------------------
-- 🎯 PROCESAR TODOS LOS PROYECTILES
-- ---------------------------------------------------------
processProjectiles 
  :: [Projectile]  -- proyectiles actuales
  -> [Enemy]       -- enemigos actuales
  -> ([Projectile], [Enemy])  
processProjectiles projectiles enemies =
  foldl step ([], enemies) projectiles
  where
    step (projs, ens) proj =
      let pos' = moveProjectile proj
      in if hasHit proj pos'
            then (projs, applyDamage proj ens) -- proyectil impacta y desaparece
            else (projs ++ [proj { projPos = pos' }], ens)

-- ---------------------------------------------------------
-- 🚀 MOVER PROYECTIL HACIA SU OBJETIVO
-- ---------------------------------------------------------
moveProjectile :: Projectile -> Pos
moveProjectile proj =
  let V2 px py = projPos proj
      V2 tx ty = enemyPos (projTarget proj)
      vx = tx - px
      vy = ty - py
      len = sqrt (vx*vx + vy*vy) + 0.0001
      dir = V2 (vx / len) (vy / len)
  in projPos proj + fmap (* projSpeed proj) dir

-- ---------------------------------------------------------
-- 💥 ¿PROYECTIL IMPACTÓ AL ENEMIGO?
-- ---------------------------------------------------------
hasHit :: Projectile -> Pos -> Bool
hasHit proj newPos =
  distance newPos (enemyPos (projTarget proj)) < 5  -- radio de impacto

-- ---------------------------------------------------------
-- 💣 APLICAR DAÑO SEGÚN TIPO DE TORRE
-- ---------------------------------------------------------
applyDamage :: Projectile -> [Enemy] -> [Enemy]
applyDamage proj enemies =
  let e = projTarget proj
  in case enemyType e of
      _ -> applyDirectDamage proj enemies


-- =========================================================
-- 1️⃣ DAÑO DIRECTO (BASIC / SNIPER)
-- =========================================================
applyDirectDamage :: Projectile -> [Enemy] -> [Enemy]
applyDirectDamage proj enemies =
  map update enemies
  where
    eid = enemyId (projTarget proj)
    dmg = projDamage proj

    update enemy
      | enemyId enemy == eid =
          let hp' = enemyHP enemy - dmg
          in if hp' <= 0 then enemy { enemyHP = 0 } else enemy { enemyHP = hp' }
      | otherwise = enemy

-- (Opcional si luego agregas SPLASH o FREEZE)
-- =========================================================
-- 2️⃣ DAÑO EN ÁREA (SPLASH)
-- =========================================================
applySplashDamage :: Projectile -> [Enemy] -> [Enemy]
applySplashDamage proj enemies =
  let center = enemyPos (projTarget proj)
      radius = 40
      dmg    = projDamage proj
  in map (splashHit center radius dmg) enemies

splashHit :: Pos -> Double -> Damage -> Enemy -> Enemy
splashHit center radius dmg e
  | distance center (enemyPos e) <= radius =
      let hp' = enemyHP e - dmg
      in if hp' <= 0 then e { enemyHP = 0 } else e { enemyHP = hp' }
  | otherwise = e

-- =========================================================
-- 3️⃣ RALENTIZACIÓN (FREEZE)
-- =========================================================
applyFreezeEffect :: Projectile -> [Enemy] -> [Enemy]
applyFreezeEffect proj enemies =
  let e = projTarget proj
  in map (freezeEnemy e) enemies

freezeEnemy :: Enemy -> Enemy -> Enemy
freezeEnemy target e
  | enemyId e == enemyId target =
      e { enemySpeed = enemySpeed e * 0.6 }  -- reduce velocidad 40%
  | otherwise = e
