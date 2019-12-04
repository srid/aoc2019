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
      ans1 = length $ filter (isPassword (> 1)) input
      ans2 = length $ filter (isPassword (== 2)) input
  print $ assert (ans1 == 1675) ans1
  print $ assert (ans2 == 1142) ans2

isPassword :: (Int -> Bool) -> String -> Bool
isPassword f s =
  and
    [ all (uncurry (<=)) $ zip s (drop 1 s),
      any (f . length) $ group s
    ]
