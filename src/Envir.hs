module Envir (EnvRef, new, branch, Envir.fromList, Envir.lookup, Envir.insert, findEnv) where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map

type EnvRef t = IORef (LocalEnv t)

data LocalEnv t = LocalEnv
  { local :: Map.Map String t,
    parent :: Maybe (EnvRef t)
  }

new :: IO (EnvRef t)
new =
  newIORef $ LocalEnv Map.empty Nothing

fromList :: [(String, t)] -> IO (EnvRef t)
fromList list =
  newIORef $ LocalEnv (Map.fromList list) Nothing

branch :: EnvRef t -> IO (EnvRef t)
branch env =
  newIORef $ LocalEnv Map.empty (Just env)

lookup :: String -> EnvRef t -> IO (Maybe t)
lookup k ref = do
  e <- readIORef ref
  case (Map.lookup k (local e), parent e) of
    (Nothing, Just p) -> Envir.lookup k p
    (Nothing, Nothing) -> return Nothing
    (v, _) -> return v

insertToLocal :: String -> t -> LocalEnv t -> LocalEnv t
insertToLocal k v env =
  let m = Map.insert k v (local env)
   in env {local = m}

insert :: String -> t -> EnvRef t -> IO t
insert k v ref = do
  modifyIORef ref $ insertToLocal k v
  return v

findEnv :: String -> EnvRef t -> IO (Maybe (EnvRef t))
findEnv k ref = do
  e <- readIORef ref
  if Map.member k (local e)
    then return $ Just ref
    else case parent e of
      Just ref -> findEnv k ref
      Nothing -> return Nothing
