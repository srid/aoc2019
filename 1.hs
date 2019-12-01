#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.shower])"
#! nix-shell -i "ghcid -T main"

{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  input :: [Int] <- fmap read . lines <$> readFile "1.input"
  print $ sum $ fuelRequired <$> input

fuelRequired :: Int -> Int
fuelRequired mass = floor (fromIntegral mass / 3 :: Double) - 2
