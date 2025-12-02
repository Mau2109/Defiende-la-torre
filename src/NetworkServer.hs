{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NetworkServer (startServer) where

import Types
import qualified WaveSystem as WS
import qualified TowerSystem as TS
import qualified ProjectileSystem as PS
import qualified EnemyMovement as EM
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import GHC.Generics
import Control.Concurrent
import Control.Monad (forever, when)
import Linear.V2 (V2(..))

--------------------------------------------------------------------------------
-- COMANDOS DESDE PYTHON
--------------------------------------------------------------------------------

data GameCommand
  = PlaceTower { cmdX :: Double, cmdY :: Double, cmdType :: String }
  | StartWave
  | UpgradeTower { cmdTowerId :: Int }
  | GetState
  | Tick { cmdDeltaTime :: Double }
  deriving (Generic, Show)

instance FromJSON GameCommand where
  parseJSON = withObject "GameCommand" $ \v -> do
    cmdType <- v .: "type"
    case cmdType :: String of
      "PlaceTower" -> PlaceTower
        <$> v .: "cmdX"
        <*> v .: "cmdY"
        <*> v .: "cmdType"
      "StartWave" -> pure StartWave
      "GetState" -> pure GetState
      "Tick" -> Tick <$> v .: "cmdDeltaTime"
      _ -> fail "Unknown command type"

instance ToJSON GameCommand

--------------------------------------------------------------------------------
-- RESPUESTAS A PYTHON
--------------------------------------------------------------------------------

data GameStateResponse = GameStateResponse
  { rspPath :: [(Double, Double)]
  , rspEnemies :: [EnemyData]
  , rspTowers :: [TowerData]
  , rspProjectiles :: [ProjectileData]
  , rspLives :: Int
  , rspMoney :: Int
  , rspWaveNumber :: Int
  , rspWaveActive :: Bool
  } deriving (Generic, Show)

data EnemyData = EnemyData
  { edId :: Int
  , edType :: String
  , edX :: Double
  , edY :: Double
  , edHP :: Int
  , edMaxHP :: Int
  } deriving (Generic, Show)

data TowerData = TowerData
  { tdX :: Double
  , tdY :: Double
  , tdType :: String
  , tdLevel :: Int
  , tdCooldown :: Int
  , tdRange :: Double
  , tdDamage :: Int
  } deriving (Generic, Show)

data ProjectileData = ProjectileData
  { pdX :: Double
  , pdY :: Double
  , pdTargetX :: Double
  , pdTargetY :: Double
  , pdDamage :: Int
  } deriving (Generic, Show)

instance ToJSON GameStateResponse
instance ToJSON EnemyData
instance ToJSON TowerData
instance ToJSON ProjectileData

--------------------------------------------------------------------------------
-- CONVERSIÓN DE ESTADO DEL JUEGO A JSON
--------------------------------------------------------------------------------

gameStateToResponse :: GameState -> GameStateResponse
gameStateToResponse gs = GameStateResponse
  { rspPath = map (\(V2 x y) -> (x, y)) (gsPath gs)
  , rspEnemies = map enemyToData (gsEnemies gs)
  , rspTowers = map towerToData (gsTowers gs)
  , rspProjectiles = map projectileToData (gsProjectiles gs)
  , rspLives = gsLives gs
  , rspMoney = gsGold gs
  , rspWaveNumber = gsWave gs
  , rspWaveActive = not (null (gsEnemies gs))
  }

enemyToData :: Enemy -> EnemyData
enemyToData e = EnemyData
  { edId = enemyId e
  , edType = show (enemyType e)
  , edX = let V2 x _ = enemyPos e in x
  , edY = let V2 _ y = enemyPos e in y
  , edHP = enemyHP e
  , edMaxHP = enemyMaxHP e
  }

towerToData :: Tower -> TowerData
towerToData t = TowerData
  { tdX = let V2 x _ = towerPos t in x
  , tdY = let V2 _ y = towerPos t in y
  , tdType = show (towerType t)
  , tdLevel = towerLevel t
  , tdCooldown = towerCooldown t
  , tdRange = towerRange t
  , tdDamage = towerDamage t
  }

projectileToData :: Projectile -> ProjectileData
projectileToData p = ProjectileData
  { pdX = let V2 x _ = projPos p in x
  , pdY = let V2 _ y = projPos p in y
  , pdTargetX = let V2 x _ = enemyPos (projTarget p) in x
  , pdTargetY = let V2 _ y = enemyPos (projTarget p) in y
  , pdDamage = projDamage p
  }

--------------------------------------------------------------------------------
-- SERVIDOR DE RED
--------------------------------------------------------------------------------

startServer :: Int -> GameState -> IO ()
startServer port initialState = do
  addr <- resolve (show port)
  sock <- open addr
  listen sock 5
  putStrLn $ "Servidor Haskell escuchando en puerto " ++ show port ++ "..."
  
  forever $ do
    (conn, _) <- accept sock
    putStrLn "Cliente conectado desde Python GUI"
    
    stateVar <- newMVar initialState
    
    -- Enviar estado inicial
    putStrLn "Enviando estado inicial al cliente..."
    let initialResponse = gameStateToResponse initialState
    sendAll conn (BL.toStrict $ encode initialResponse <> "\n")
    
    -- Loop de comunicación
    handleClient conn stateVar
    
    close conn

handleClient :: Socket -> MVar GameState -> IO ()
handleClient conn stateVar = do
  msg <- recv conn 4096
  if BS.null msg
    then putStrLn "Cliente desconectado"
    else do
      -- Filtrar solo líneas que parezcan JSON
      let jsonLines = filter isJsonLike (BS.lines msg)
      
      if null jsonLines
        then handleClient conn stateVar
        else do
          let jsonMsg = head jsonLines
          case eitherDecodeStrict jsonMsg of
            Left err -> do
              putStrLn $ "Error decodificando JSON: " ++ err
              putStrLn $ "Mensaje JSON: " ++ BS.unpack jsonMsg
              handleClient conn stateVar
            Right cmd -> do
              -- Solo imprimir comandos importantes (no Tick)
              case cmd of
                Tick _ -> return ()  -- No imprimir ticks
                _ -> putStrLn $ "Comando: " ++ show cmd
              
              response <- handleCommand stateVar cmd
              sendAll conn (BL.toStrict $ encode response <> "\n")
              handleClient conn stateVar

-- Verificar si una línea parece JSON
isJsonLike :: BS.ByteString -> Bool
isJsonLike bs = 
  let s = BS.unpack bs
  in not (null s) && head s == '{'

resolve :: String -> IO AddrInfo
resolve port = do
  let hints = defaultHints { addrSocketType = Stream }
  head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  return sock

--------------------------------------------------------------------------------
-- MANEJO DE COMANDOS
--------------------------------------------------------------------------------

handleCommand :: MVar GameState -> GameCommand -> IO GameStateResponse
handleCommand stateVar cmd = do
  modifyMVar stateVar $ \state -> do
    let newState = case cmd of
          Tick dt -> updateGame dt state
          StartWave -> WS.startNextWave state
          PlaceTower x y tType -> placeTowerWrapper state (V2 x y) tType
          GetState -> state
          _ -> state
    
    return (newState, gameStateToResponse newState)

-- Actualizar el juego usando tus módulos existentes
updateGame :: Double -> GameState -> GameState
updateGame dt state =
  let -- Primero mover enemigos (usando tu función stepGame)
      state1 = EM.stepGame dt state
      
      -- Torres disparan (usando tu TowerSystem)
      state2 = TS.updateTowers state1
      
      -- Actualizar proyectiles (usando tu ProjectileSystem)
      state3 = PS.updateProjectiles state2
      
      -- Actualizar tiempo
      state4 = state3 { gsTime = gsTime state3 + dt }
  in state4

-- Colocar torre usando tu TowerSystem
placeTowerWrapper :: GameState -> Pos -> String -> GameState
placeTowerWrapper state pos towerTypeStr =
  let tType = parseTowerType towerTypeStr
  in TS.placeTower tType pos state

parseTowerType :: String -> TowerType
parseTowerType "Basic" = Basic
parseTowerType "Sniper" = Sniper
parseTowerType "Freeze" = Freeze
parseTowerType "Splash" = Splash
parseTowerType _ = Basic