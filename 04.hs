#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE NoImplicitPrelude #-}

import Control.Exception (assert)
import Relude

main :: IO ()
main = do
  let input = show <$> [172930 .. 683082 :: Int]
      ans1 = length $ filter (isPassword (>=)) input
      ans2 = length $ filter (isPassword (==)) input
  print $ assert (ans1 == 1675) ans1
  print $ assert (ans2 == 1142) ans2

isPassword :: (Int -> Int -> Bool) -> String -> Bool
isPassword cmp s =
  and
    [ all (uncurry (<=)) $ zip s (drop 1 s),
      any ((`cmp` 2) . length) $ group s
    ]
