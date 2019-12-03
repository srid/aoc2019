#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude p.megaparsec])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Exception
import qualified Data.Map as Map
import Relude
import Relude.Extra.Bifunctor
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Move
  = R Int
  | U Int
  | L Int
  | D Int
  deriving (Eq, Show)

type Coord = (Int, Int)

-- | A path is set of coordinates and the signal delay length (sum of steps)
type Path = Map Coord Int

moveLength :: Move -> Int
moveLength = \case
  R n -> n
  U n -> n
  L n -> n
  D n -> n

moveCoord :: Coord -> Move -> Coord
moveCoord (x, y) = bimap (+x) (+y) . \case
  R n -> (n, 0)
  U n -> (0, -n)
  L n -> (-n, 0)
  D n -> (0, n)

movePath :: Move -> Path
movePath = \case
  R n -> coords [0 .. n] [0]
  U n -> coords [0] [0, -1 .. (- n)]
  L n -> coords [0, -1 .. (- n)] [0]
  D n -> coords [0] [0 .. n]
  where
    coords x y = Map.fromList $ liftA2 (,) x y <&> \(a, b) -> ((a, b), abs a + abs b)

main :: IO ()
main = do
  input <- readFileText "input/3"
  forM_ (samples <> [(input, (280, 10554))]) $ \(inp, (part1Ans, part2Ans)) -> do
    let (pathA, pathB) = bimapBoth mkPath $ parseInput inp
    let common = Map.delete ((0, 0)) $ Map.intersectionWith (+) pathA pathB
    let part1 = head $ fromList $ sort $ map (\(a, b) -> abs a + abs b) $ Map.keys common
        part2 = head $ fromList $ sort $ Map.elems common
    putStrLn "Answer:"
    print $ assert (part1 == part1Ans) part1
    print $ assert (part2 == part2Ans) part2
  where
    samples =
      [ -- 6
        ("R8,U5,L5,D3\nU7,R6,D4,L4", (6, 30)),
        -- 159
        ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", (159, 610)),
        -- 135
        ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", (135, 410))
      ]

mkPath :: [Move] -> Path
mkPath = snd . foldl' f (((0, 0), 0), mempty)
  where
    f ((curr, dist), acc) move =
      let next = shiftPath curr dist (movePath move)
       in ((moveCoord curr move, dist + moveLength move), Map.unionWith min next acc)
    shiftPath (x, y) dist = Map.fromList . fmap (bimap (bimap (+ x) (+ y)) (+ dist)) . Map.toList

parseInput :: Text -> ([Move], [Move])
parseInput =
  either (error . toText . errorBundlePretty) id
    . parse inputParser "someFile"

inputParser :: Parser ([Move], [Move])
inputParser = (,) <$> (p <* eol) <*> p
  where
    p = sepBy1 moveParser (char ',')

moveParser :: Parser Move
moveParser = choice $
  dirs <&> \(c, mk) -> do
    void $ char c
    mk <$> L.decimal
  where
    dirs = [('R', R), ('U', U), ('L', L), ('D', D)]
