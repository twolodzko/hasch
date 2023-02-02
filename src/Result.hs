module Result (Result (..), (>>>=), isErr) where

data Result t
  = Ok t
  | Err String
  deriving (Eq, Show)

(>>>=) :: IO (Result a) -> (a -> IO (Result b)) -> IO (Result b)
x >>>= f =
  x >>= go f
  where
    go f (Ok x) = f x
    go _ (Err msg) = return $ Err msg

isErr :: Result t -> Bool
isErr (Err _) = True
isErr (Ok _) = False
