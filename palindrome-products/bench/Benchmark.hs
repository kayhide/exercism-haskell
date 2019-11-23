import Palindromes (largestPalindrome, smallestPalindrome)

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Criterion.Types (Benchmark)


benchGroup :: (String, Integer, Integer) -> Benchmark
benchGroup (desc, min', max') =
  bgroup desc
  [ bench "smallestPalindrome" $ nf (smallestPalindrome min') max'
  , bench "largestPalindrome" $ nf (largestPalindrome min') max'
  ]

main :: IO ()
main = do
  defaultMain $ benchGroup <$> cases
  where

    cases = [ ("palindromes from single digit factors",     1,     9)
            , ("palindromes from double digit factors",    10,    99)
            , ("palindromes from triple digit factors",   100,   999)
            , ("palindromes from four digit factors"  ,  1000,  9999)
            ]
