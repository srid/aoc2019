#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.relude])"
#! nix-shell -i "ghcid -T main"

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Monad (guard)
import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Relude
import Relude.Extra.Tuple (dupe)

main :: IO ()
main = do
  input :: [Int] <- fmap read . lines <$> readFileText "1.input"
  -- Expect: 5277255
  print $ sum $ completeFuelRequired <$> input

completeFuelRequired :: Int -> Int
completeFuelRequired = sum . unfoldr go
  where
    go n = do
      let v = fuelRequired n
      guard $ v > 0
      pure $ dupe v

fuelRequired :: Int -> Int
fuelRequired mass =
  floor (fromIntegral mass / 3 :: Double) - 2

-- | Bypass relude's safety
read :: Read a => Text -> a
read = fromJust . readMaybe . toString
