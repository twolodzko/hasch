{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module FileReader (FileReaderRef, new) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Parser (Reader (peek, pop))
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, openFile)
import Text.Printf (printf)

data FileReader = FileReader
  { cache :: Maybe String,
    file :: Handle
  }

readLine :: FileReader -> IO FileReader
readLine r@(FileReader {cache = _, file = file}) = do
  iseof <- hIsEOF file
  if iseof
    then return $ r {cache = Nothing}
    else do
      line <- hGetLine file
      return $ r {cache = Just $ '\n' : line}

type FileReaderRef = IORef FileReader

new :: String -> IO FileReaderRef
new name = do
  handle <- openFile name ReadMode
  newIORef FileReader {cache = Just "", file = handle}

instance Reader FileReaderRef where
  peek :: FileReaderRef -> IO (Maybe Char)
  peek ref = do
    r <- readIORef ref
    case r of
      FileReader {cache = Nothing} ->
        return Nothing
      FileReader {cache = Just (x : _), file = _} ->
        return $ Just x
      FileReader {cache = Just "", file = file} -> do
        writeIORef ref =<< readLine r
        peek ref

  pop :: FileReaderRef -> IO (Maybe Char)
  pop ref = do
    r <- readIORef ref
    case r of
      FileReader {cache = Nothing} ->
        return Nothing
      FileReader {cache = Just (x : xs), file = _} -> do
        writeIORef ref $ r {cache = Just xs}
        return $ Just x
      FileReader {cache = Just "", file = file} -> do
        writeIORef ref =<< readLine r
        pop ref
