{-# LANGUAGE ScopedTypeVariables #-}
module ArmstrongNumbers (armstrong) where

armstrong :: forall a. Integral a => a -> Bool
armstrong n = (== n) . sum $ (^ len) <$> digits
  where
    len :: Int
    len = length digits

    digits :: [a]
    digits =
      fmap snd
      $ takeWhile (\(x, y) -> x > 0 || y > 0)
      $ drop 1
      $ iterate ((`divMod` 10) . fst) (n, 0)
