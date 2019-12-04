#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Exception (assert)
import Relude

main :: IO ()
main = do
  let (a, b, ans) = (172930 :: Int, 683082, 1675)
      myAns = length $ filter isPassword $ show <$> [a .. b]
  print $ assert (ans == myAns) myAns

isPassword :: String -> Bool
isPassword s =
  and
    [ any (uncurry (==)) pairs,
      all (uncurry (<=)) pairs
    ]
  where
    pairs = zip s (drop 1 s)
