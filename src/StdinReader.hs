{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module StdinReader (StdinReader, StdinReader.new) where

import Parser (Reader (peek, pop))
import StringReader (StringReader, new, set)
import System.IO (hFlush, stdout)

newtype StdinReader = StdinReader StringReader

new :: IO StdinReader
new = do
  reader <- StringReader.new ""
  return $ StdinReader reader

instance Reader StdinReader where
  peek :: StdinReader -> IO (Maybe Char)
  peek r@(StdinReader ref) = do
    result <- peek ref
    case result of
      Just c -> return $ Just c
      Nothing -> do
        readPrompt ref
        peek r

  pop :: StdinReader -> IO (Maybe Char)
  pop r@(StdinReader ref) = do
    result <- pop ref
    case result of
      Just c -> return $ Just c
      Nothing -> do
        readPrompt ref
        pop r

readPrompt :: StringReader -> IO ()
readPrompt ref = do
  putStr "> "
  hFlush stdout
  StringReader.set ref =<< getLine
