module Prime (nth) where

import qualified Data.Set as Set
import Data.Set (Set, lookupGT, (\\))

nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = pure . Set.elemAt (n - 1) . head . dropWhile ((< n) . Set.size) . iterate step $ Set.fromList [2, 3, 5, 7]



step :: Set Integer -> Set Integer
step current = f 2 $ current <> next
  where
    lo :: Integer
    lo = Set.findMax current

    hi :: Integer
    hi = lo * 2

    next :: Set Integer
    next = Set.fromList [lo .. hi]

    f :: Integer -> Set Integer -> Set Integer
    f x xs = maybe id f =<< lookupGT x $ sieve x xs

    sieve :: Integer -> Set Integer -> Set Integer
    sieve x = (\\ Set.fromList (filter (lo <) [x * 2, x * 3 .. hi]))
