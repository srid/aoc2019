#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude p.megaparsec])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Exception (assert)
import qualified Data.Map as Map
import GHC.Natural
import Relude
import Relude.Extra.Bifunctor (bimapBoth)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Move
  = R Natural
  | U Natural
  | L Natural
  | D Natural
  deriving (Eq, Show)

type Point = (Int, Int)

-- | A path is map of points to their signal delay length
type Path = Map Point Natural

distance :: Point -> Natural
distance = uncurry (+) . bimapBoth abs'

shiftPoint :: Point -> Point -> Point
shiftPoint (x, y) = bimap (+ x) (+ y)

pointFromOrigin :: Move -> Point
pointFromOrigin = \case
  R n -> (naturalToInt n, 0)
  U n -> (0, - naturalToInt n)
  L n -> (- naturalToInt n, 0)
  D n -> (0, naturalToInt n)

pointsFromOrigin :: Move -> [Point]
pointsFromOrigin = uncurry (liftA2 (,)) . bimapBoth range . pointFromOrigin
  where
    range 0 = [0]
    range b = [0, signum b .. b]

pointFrom :: Point -> Move -> Point
pointFrom p = shiftPoint p . pointFromOrigin

moveLength :: Move -> Natural
moveLength = distance . pointFromOrigin

pathFromOrigin :: Move -> Path
pathFromOrigin = Map.fromList . fmap (id &&& distance) . pointsFromOrigin

pathFrom :: Point -> Natural -> Move -> Path
pathFrom loc dist = shiftPath loc dist . pathFromOrigin

shiftPath :: Point -> Natural -> Path -> Path
shiftPath (x, y) dist =
  Map.mapKeys (bimap (+ x) (+ y)) . Map.map (+ dist)

origin :: Point
origin = (0, 0)

mkPath :: [Move] -> Path
mkPath = Map.delete origin . snd . foldl' f ((origin, 0), mempty)
  where
    f ((loc, dist), acc) move =
      ( (pointFrom loc move, dist + moveLength move),
        acc `Map.union` pathFrom loc dist move
      )

main :: IO ()
main = do
  input <- (,(280, 10554)) <$> readFileText "input/3"
  forM_ (samples <> [input]) $ \(inp, (part1Ans, part2Ans)) -> do
    let (pathA, pathB) = bimapBoth mkPath $ parseInput inp
    let common = Map.intersectionWith (+) pathA pathB
    let part1 = minimumElem $ distance <$> Map.keys common
        part2 = minimumElem $ Map.elems common
    putStrLn "Answer:"
    print $ assert (part1 == part1Ans) part1
    print $ assert (part2 == part2Ans) part2
  where
    samples =
      [ ("R8,U5,L5,D3\nU7,R6,D4,L4", (6, 30)),
        ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", (159, 610)),
        ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", (135, 410))
      ]

abs' :: Int -> Natural
abs' = intToNatural . abs

minimumElem :: Ord a => [a] -> a
minimumElem = head . fromList . sort

parseInput :: Text -> ([Move], [Move])
parseInput =
  either (error . toText . errorBundlePretty) id
    . parse inputParser "someFile"

inputParser :: Parser ([Move], [Move])
inputParser = (,) <$> (p <* eol) <*> p
  where
    p = sepBy1 moveParser (char ',')

moveParser :: Parser Move
moveParser =
  choice $
    dirs <&> \(c, mk) -> do
      void $ char c
      mk <$> L.decimal
  where
    dirs = [('R', R), ('U', U), ('L', L), ('D', D)]
