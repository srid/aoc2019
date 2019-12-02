#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude p.megaparsec])"
#! nix-shell --pure -i "ghcid -T main"

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
  nums <-
    readFileText "input/2"
      <&> either (error . toText . errorBundlePretty) id
      . parse inputParser "input/2"
  print nums

inputParser :: Parser [Int]
inputParser = sepBy1 L.decimal (char ',')
