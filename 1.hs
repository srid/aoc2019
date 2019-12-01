#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [])"
#! nix-shell -i "ghcid -T main"

{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  input :: [Int] <- fmap read . lines <$> readFile "input/1"
  print $ sum $ fuelRequired <$> input

fuelRequired :: Int -> Int
fuelRequired mass = floor (fromIntegral mass / 3 :: Double) - 2
