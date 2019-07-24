module School (School, add, empty, grade, sorted) where

import Control.Arrow ((&&&))
import Data.List (nub, sort)

type School = [(Int, String)]

add :: Int -> String -> School -> School
add gradeNum student school = (gradeNum, student) : school

empty :: School
empty = []

grade :: Int -> School -> [String]
grade gradeNum school = sort $ snd <$> filter ((== gradeNum) . fst) school

sorted :: School -> [(Int, [String])]
sorted school = (id &&& flip grade school) <$> grades
  where
    grades :: [Int]
    grades = nub . sort $ fst <$> school
