module Parser (Reader (peek, pop), parse) where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Types (Error, Sexpr (..))

class Reader r where
  peek :: r -> IO (Maybe Char)
  pop :: r -> IO (Maybe Char)

type ParsingResult = ExceptT Error IO (Maybe Sexpr)

parse :: Reader r => r -> ParsingResult
parse r = do
  result <- liftIO $ peek r
  case result of
    Just x | isSpace x -> popAnd r parse
    Just '(' -> popAnd r $ parseList []
    Just ')' -> throwE "unexpected )"
    Just '\'' -> popAnd r parseQuoted
    Just ',' -> throwE "not implemented"
    Just '"' -> popAnd r $ parseString ""
    Just ';' -> do
      liftIO $ skipLine r
      parse r
    Just _ -> parseWord r
    Nothing -> return Nothing

popAnd :: Reader r => r -> (r -> ParsingResult) -> ParsingResult
popAnd r f = do
  liftIO $ pop r
  f r

parseList :: Reader r => [Sexpr] -> r -> ParsingResult
parseList acc r = do
  result <- liftIO $ peek r
  case result of
    Just ')' -> do
      liftIO $ pop r
      return $ Just $ List $ reverse acc
    Just c | isSpace c -> popAnd r $ parseList acc
    Just ';' -> do
      liftIO $ skipLine r
      parseList acc r
    _ -> do
      result <- liftIO $ runExceptT $ parse r
      case result of
        Right (Just sexpr) -> parseList (sexpr : acc) r
        Right Nothing -> throwE "missing )"
        Left msg -> throwE msg

parseString :: Reader r => String -> r -> ParsingResult
parseString acc r = do
  result <- liftIO $ pop r
  case result of
    Just '"' -> return $ Just $ String $ reverse acc
    Just c -> parseString (c : acc) r
    Nothing -> throwE "missing \""

parseQuoted :: Reader r => r -> ParsingResult
parseQuoted r = do
  result <- liftIO $ runExceptT $ parse r
  case result of
    Right (Just sexpr) -> return $ Just $ Quote sexpr
    Right Nothing -> throwE "missing quoted value"
    Left err -> throwE err

parseWord :: Reader r => r -> ParsingResult
parseWord r = do
  result <- liftIO $ readWord r ""
  return $ maybeTransform [maybeBool, maybeInt, maybeFloat, Just . Symbol] result

readWord :: Reader r => r -> String -> IO String
readWord r acc = do
  result <- liftIO $ peek r
  case result of
    Just c | isWordEnd c -> return $ reverse acc
    Just c -> do
      liftIO $ pop r
      readWord r (c : acc)
    Nothing -> return $ reverse acc

maybeTransform :: [String -> Maybe Sexpr] -> String -> Maybe Sexpr
maybeTransform (f : fx) x =
  case f x of
    Just v -> Just v
    Nothing -> maybeTransform fx x
maybeTransform [] x = undefined

maybeBool :: String -> Maybe Sexpr
maybeBool "#t" = Just $ Bool True
maybeBool "#f" = Just $ Bool False
maybeBool s = Nothing

maybeInt :: String -> Maybe Sexpr
maybeInt s = Int <$> (readMaybe s :: Maybe Int)

maybeFloat :: String -> Maybe Sexpr
maybeFloat s = Float <$> (readMaybe s :: Maybe Float)

isWordEnd :: Char -> Bool
isWordEnd ch =
  isSpace ch
    || ch `elem` ['(', ')', ',', '\'', '"']

skipLine :: Reader r => r -> IO ()
skipLine r = do
  result <- liftIO $ pop r
  case result of
    Just '\n' -> return ()
    Nothing -> return ()
    Just _ -> skipLine r
