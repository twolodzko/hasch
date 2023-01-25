{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module StringReader (StringReader, new, set) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Parser (Reader (peek, pop))

type StringReader = IORef String

new :: String -> IO StringReader
new = newIORef

set :: StringReader -> String -> IO ()
set ref new =
  writeIORef ref $ '\n' : new

instance Reader StringReader where
  peek :: StringReader -> IO (Maybe Char)
  peek ref = do
    r <- readIORef ref
    case r of
      (x : _) -> return $ Just x
      [] -> return Nothing

  pop :: StringReader -> IO (Maybe Char)
  pop ref = do
    r <- readIORef ref
    case r of
      (x : xs) -> do
        writeIORef ref xs
        return $ Just x
      [] -> return Nothing
