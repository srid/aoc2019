#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude])"
#! nix-shell --pure -i "ghcid -T main"

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Relude
import Relude.Extra.Tuple (dup)

main :: IO ()
main = do
  Right input <- readLines "input/1"
  print $ sum $ completeFuelRequired <$> input
  -- Expect: 5277255

completeFuelRequired :: Int -> Int
completeFuelRequired =
  sum . unfoldr (fmap dup . guarded (> 0) . fuelRequired)

fuelRequired :: Int -> Int
fuelRequired mass =
  floor (fromIntegral mass / 3 :: Double) - 2

readLines :: Read a => FilePath -> IO (Either Text [a])
readLines f =
  traverse readEither . fmap toString . lines <$> readFileText f
