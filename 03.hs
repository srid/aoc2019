#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude p.megaparsec])"
#! nix-shell --pure -i "ghcid -T main"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Relude hiding (some)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

main :: IO ()
main = do
  nums <- restore1202 <$> readInput "input/2"
  print nums

restore1202 :: [Int] -> [Int]
restore1202 = \case
  a : _ : _ : xs -> a : 12 : 2 : xs
  _ -> error "Bad input"

readInput :: FilePath -> IO [Int]
readInput f =
  readFileText f
    <&> either (error . toText . errorBundlePretty) id
    . parse inputParser f

inputParser :: Parser [Int]
inputParser = sepBy1 L.decimal (char ',')
