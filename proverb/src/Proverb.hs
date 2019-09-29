module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite items = intercalate "\n" $ body <> last
  where
    body :: [String]
    body = zipWith for_want_of_ items $ tail items

    last :: [String]
    last = take 1 $ and_all_for_want_of_ <$> items



for_want_of_ :: String -> String -> String
for_want_of_ x y = unwords ["For want of a", x, "the", y, "was lost."]

and_all_for_want_of_ :: String -> String
and_all_for_want_of_ x = unwords ["And all for the want of a", x <> "."]
