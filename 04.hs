#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE NoImplicitPrelude #-}

import Control.Exception (assert)
import Relude

main :: IO ()
main = do
  let ans = length $ filter isPassword $ show <$> [172930 .. 683082 :: Int]
  print $ assert (ans == 1675) ans

isPassword :: String -> Bool
isPassword s =
  and
    [ any (uncurry (==)) pairs,
      all (uncurry (<=)) pairs
    ]
  where
    pairs = zip s (drop 1 s)
