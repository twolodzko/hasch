module Parser (Reader (peek, pop), parse) where

import Data.Char (isSpace)
import Text.Read (readMaybe)
import Types (Result (Err, Ok), Sexpr (..))

class Reader r where
  peek :: r -> IO (Maybe Char)
  pop :: r -> IO (Maybe Char)

type ParsingResult = IO (Result (Maybe Sexpr))

parse :: Reader r => r -> ParsingResult
parse r = do
  result <- peek r
  case result of
    Just x | isSpace x -> popAnd r parse
    Just '(' -> popAnd r $ parseList []
    Just ')' -> return $ Err "unexpected )"
    Just '\'' -> popAnd r parseQuoted
    Just ',' -> return $ Err "not implemented"
    Just '"' -> popAnd r $ parseString ""
    Just ';' -> do
      skipLine r
      parse r
    Just _ -> parseWord r
    Nothing -> return $ Ok Nothing

popAnd :: Reader r => r -> (r -> ParsingResult) -> ParsingResult
popAnd r f = do
  pop r
  f r

parseList :: Reader r => [Sexpr] -> r -> ParsingResult
parseList acc r = do
  result <- peek r
  case result of
    Just ')' -> do
      pop r
      return $ Ok $ Just $ List $ reverse acc
    Just c | isSpace c -> popAnd r $ parseList acc
    Just ';' -> do
      skipLine r
      parseList acc r
    _ -> do
      result <- parse r
      case result of
        Ok (Just sexpr) -> parseList (sexpr : acc) r
        Ok Nothing -> return $ Err "missing )"
        Err msg -> return $ Err msg

parseString :: Reader r => String -> r -> ParsingResult
parseString acc r = do
  result <- pop r
  case result of
    Just '"' -> return $ Ok $ Just $ String $ reverse acc
    Just c -> parseString (c : acc) r
    Nothing -> return $ Err "missing \""

parseQuoted :: Reader r => r -> ParsingResult
parseQuoted r = do
  result <- parse r
  return $ case result of
    Ok (Just sexpr) -> Ok $ Just $ Quote sexpr
    Ok Nothing -> Err "missing quoted value"
    err -> err

parseWord :: Reader r => r -> ParsingResult
parseWord r = do
  result <- readWord r ""
  return $ Ok $ Just $ maybeBool result

readWord :: Reader r => r -> String -> IO String
readWord r acc = do
  result <- peek r
  case result of
    Just c | isWordEnd c -> return $ reverse acc
    Just c -> do
      pop r
      readWord r (c : acc)
    Nothing -> return $ reverse acc

maybeBool :: String -> Sexpr
maybeBool "#t" = Bool True
maybeBool "#f" = Bool False
maybeBool s = maybeInt s

maybeInt :: String -> Sexpr
maybeInt s =
  case (readMaybe s :: Maybe Int) of
    Just num -> Int num
    Nothing -> maybeFloat s

maybeFloat :: String -> Sexpr
maybeFloat s =
  case (readMaybe s :: Maybe Float) of
    Just num -> Float num
    Nothing -> Symbol s

isWordEnd :: Char -> Bool
isWordEnd ch =
  isSpace ch
    || ch `elem` ['(', ')', ',', '\'', '"']

skipLine :: Reader r => r -> IO ()
skipLine r = do
  result <- pop r
  case result of
    Just '\n' -> return ()
    Nothing -> return ()
    Just _ -> skipLine r
