{-# LANGUAGE LambdaCase #-}
module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x = toClassification . compare x . sum <$> factors x
  where
    toClassification :: Ordering -> Classification
    toClassification = \case
      LT -> Abundant
      EQ -> Perfect
      GT -> Deficient

factors :: Int -> Maybe [Int]
factors x
  | x <= 0 = Nothing
  | otherwise = Just $ filter ((== 0) . mod x) [1 .. x `div` 2]
