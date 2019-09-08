module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA ('C' : xs) = ('G' :) <$> toRNA xs
toRNA ('G' : xs) = ('C' :) <$> toRNA xs
toRNA ('T' : xs) = ('A' :) <$> toRNA xs
toRNA ('A' : xs) = ('U' :) <$> toRNA xs
toRNA (x : _) = Left x
