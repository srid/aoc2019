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

import qualified Data.Set as Set
import Relude hiding (traceShowId)
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

type Path = Set Coord

moveCoord :: Move -> Coord -> Coord
moveCoord move (x, y) = case move of
  R n -> (x + n, y)
  U n -> (x, y - n)
  L n -> (x - n, y)
  D n -> (x, y + n)

movePath :: Move -> Path
movePath = \case
  R n -> coords [0 .. n] [0]
  U n -> coords [0] [0, -1 .. (- n)]
  L n -> coords [0, -1 .. (- n)] [0]
  D n -> coords [0] [0 .. n]
  where
    coords x y = Set.fromList $ liftA2 (,) x y

main :: IO ()
main = do
  -- 280
  input <- readFileText "input/3"
  forM_ (samples <> [input]) $ \inp -> do
    let (pathA, pathB) = bimapBoth mkPath $ parseInput inp
    let common = Set.intersection pathA pathB
    print $ Set.lookupMin $ Set.delete 0 $ Set.fromList $ uncurry (+) . bimapBoth abs <$> Set.toList common
  where
    samples =
      [ -- 6
        "R8,U5,L5,D3\nU7,R6,D4,L4",
        -- 159
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83",
        -- 135
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      ]

mkPath :: [Move] -> Path
mkPath = snd . foldl' f ((0, 0), mempty)
  where
    f (curr, acc) move =
      let next = shiftPath curr (movePath move)
       in (moveCoord move curr, Set.union next acc)
    shiftPath (x, y) = Set.fromList . fmap (bimap (+x) (+y)) . Set.toList

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
