module FoodChain (song) where

import Data.Char (toLower)
import Data.List (intercalate)

song :: String
song = intercalate "\n" $ unlines . chant <$> [Fly .. Horse]


data Creature
  = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse
  deriving (Eq, Show, Enum)


uncapitalize :: Creature -> String
uncapitalize x = toLower <$> show x


chant :: Creature -> [String]
chant x@Fly   = [i_know x, wow x]
chant x@Horse = [i_know x, wow x]
chant x       = [i_know x, wow x]
  <> map she_swallowed [x, pred x .. Spider]
  <> [wow Fly]

i_know :: Creature -> String
i_know x = "I know an old lady who swallowed a " <> uncapitalize x <> "."

wow :: Creature -> String
wow Fly    = "I don't know why she swallowed the fly. Perhaps she'll die."
wow Spider = "It wriggled and jiggled and tickled inside her."
wow Bird   = "How absurd to swallow a bird!"
wow Cat    = "Imagine that, to swallow a cat!"
wow Dog    = "What a hog, to swallow a dog!"
wow Goat   = "Just opened her throat and swallowed a goat!"
wow Cow    = "I don't know how she swallowed a cow!"
wow Horse  = "She's dead, of course!"

she_swallowed :: Creature -> String
she_swallowed x = "She swallowed the " <> uncapitalize x <> to_catch (pred x)

to_catch :: Creature -> String
to_catch Spider = " to catch the spider that wriggled and jiggled and tickled inside her."
to_catch x = " to catch the " <> uncapitalize x <> "."
