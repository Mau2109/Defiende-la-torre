{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , (.=)
  , object
  , withObject
  , (.:)
  )
import Data.Aeson.Key (fromString)
import Linear.V2 (V2(..))

--------------------------------------------------------------------------------
-- INSTANCIAS JSON PARA V2 Double (NECESARIO PARA PYTHON)
--------------------------------------------------------------------------------

instance ToJSON (V2 Double) where
  toJSON (V2 x y) =
    object
      [ "x" .= x
      , "y" .= y
      ]

instance FromJSON (V2 Double) where
  parseJSON = withObject "V2 Double" $ \o ->
    V2
      <$> o .: "x"
      <*> o .: "y"

--------------------------------------------------------------------------------
-- TYPES BÁSICOS
--------------------------------------------------------------------------------

type Time       = Double
type Health     = Int
type Damage     = Int
type Range      = Double
type Cooldown   = Int
type Gold       = Int
type WaveNumber = Int

type Pos = V2 Double
type Path = [Pos]

--------------------------------------------------------------------------------
-- TILES
--------------------------------------------------------------------------------

data Tile = Empty | TowerSpot | PathTile | Base
  deriving (Show, Eq, Generic)

instance ToJSON Tile
instance FromJSON Tile

--------------------------------------------------------------------------------
-- ENEMIES
--------------------------------------------------------------------------------

data EnemyType = Normal | Fast | Tank | Flying
  deriving (Show, Eq, Generic)

instance ToJSON EnemyType
instance FromJSON EnemyType

data Enemy = Enemy
  { enemyId       :: Int
  , enemyType     :: EnemyType
  , enemyPos      :: Pos
  , enemyPathIdx  :: Int
  , enemyHP       :: Health
  , enemyMaxHP    :: Health
  , enemySpeed    :: Double
  , enemyReward   :: Gold
  } deriving (Show, Generic)

instance ToJSON Enemy
instance FromJSON Enemy

--------------------------------------------------------------------------------
-- TOWERS
--------------------------------------------------------------------------------

data TowerType = Basic | Sniper | Freeze | Splash
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TowerType
instance FromJSON TowerType

data Tower = Tower
  { towerPos      :: Pos
  , towerType     :: TowerType
  , towerLevel    :: Int
  , towerCooldown :: Cooldown
  , towerRange    :: Range
  , towerDamage   :: Damage
  } deriving (Show, Generic)

instance ToJSON Tower
instance FromJSON Tower

--------------------------------------------------------------------------------
-- PROJECTILES
--------------------------------------------------------------------------------

data Projectile = Projectile
  { projPos    :: Pos
  , projTarget :: Enemy
  , projDamage :: Damage
  , projSpeed  :: Double
  } deriving (Show, Generic)

instance ToJSON Projectile
instance FromJSON Projectile

--------------------------------------------------------------------------------
-- GAME STATE (LO QUE PYTHON RECIBE)
--------------------------------------------------------------------------------

data GameState = GameState
  { gsPath         :: Path
  , gsEnemies      :: [Enemy]
  , gsTowers       :: [Tower]
  , gsProjectiles  :: [Projectile]
  , gsWave         :: WaveNumber
  , gsLives        :: Int
  , gsGold         :: Gold
  , gsTime         :: Time
  , gsNextEnemyId  :: Int
  } deriving (Show, Generic)

instance ToJSON GameState
instance FromJSON GameState

--------------------------------------------------------------------------------
-- ESTADO INICIAL
--------------------------------------------------------------------------------

initialGameState :: Path -> GameState
initialGameState path =
  GameState
    { gsPath         = path
    , gsEnemies      = []
    , gsTowers       = []
    , gsProjectiles  = []
    , gsWave         = 1
    , gsLives        = 20
    , gsGold         = 200
    , gsTime         = 0
    , gsNextEnemyId  = 0
    }
