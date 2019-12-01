#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude])"
#! nix-shell -i "ghcid -T main"

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Relude
import Relude.Bool.Guard (guarded)
import Relude.Extra.Tuple (dup)

main :: IO ()
main = do
  input :: [Int] <- fmap read . lines <$> readFileText "input/1"
  -- Expect: 5277255
  print $ sum $ completeFuelRequired <$> input

completeFuelRequired :: Int -> Int
completeFuelRequired =
  sum . unfoldr (fmap dup . guarded (> 0) . fuelRequired)

fuelRequired :: Int -> Int
fuelRequired mass =
  floor (fromIntegral mass / 3 :: Double) - 2

-- | Bypass relude's safety
read :: Read a => Text -> a
read = fromJust . readMaybe . toString
