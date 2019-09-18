{-# LANGUAGE LambdaCase #-}
module DNA (toRNA) where


toRNA :: String -> Either Char String
toRNA = traverse turn'

turn' :: Char -> Either Char Char
turn' = \case
  'C' -> Right 'G'
  'G' -> Right 'C'
  'T' -> Right 'A'
  'A' -> Right 'U'
  x  -> Left x
