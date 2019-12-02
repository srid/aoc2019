#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude p.megaparsec p.monad-loops])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Monad.Loops (untilJust)
import Relude
import Relude.Extra.Map
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

main :: IO ()
main = do
  forM_ sample $ \s -> do
    print $ go $ parseInput ".." s
  input <- readInput "input/2"
  -- Expect 5098658
  print $ goHead $ restore1202 input
  forM_ ((,) <$> [0 .. 99] <*> [0 .. 99]) $ \(a, b) ->
    case goHead (restoreWith a b input) of
      -- Expect: (50, 64)
      Just 19690720 -> print (a, b, 100 * a + b)
      _ -> pure ()
  where
    goHead :: [Int] -> Maybe Int
    goHead = listToMaybe . take 1 . go
    go = fst . runState computer . indexList 4
    sample =
      [ "1,0,0,0,99",
        "2,3,0,3,99",
        "2,4,4,5,99,0",
        "1,1,1,4,99,5,6,0,99"
      ]

computer :: State ([Int], IntMap Int) [Int]
computer =
  fmap elems <$> untilJust $
    get >>= \case
      ([], mem) -> pure $ Just mem
      (idx : is, mem) -> case readAddr idx mem of
        99 -> pure $ Just mem
        1 -> put (is, doOp (+) idx mem) >> pure Nothing
        2 -> put (is, doOp (*) idx mem) >> pure Nothing
        op -> error $ "Bad op code: " <> show op
  where
    readAddr :: Int -> IntMap Int -> Int
    readAddr k = fromMaybe (error "Bad addr") . lookup k
    readMem :: Int -> IntMap Int -> Int
    readMem k m = fromMaybe (error "Bad memory ref") $ lookup (readAddr k m) m
    writeMem :: Int -> Int -> IntMap Int -> IntMap Int
    writeMem k v m = alter (maybe (error "Bad memory ref") (const $ Just v)) (readAddr k m) m
    doOp :: (Int -> Int -> Int) -> Int -> IntMap Int -> IntMap Int
    doOp op idx mem =
      let a = readMem (idx + 1) mem
          b = readMem (idx + 2) mem
       in writeMem (idx + 3) (op a b) mem

indexList :: Int -> [a] -> ([Int], IntMap a)
indexList next x = (opIndices, fromList $ zip indices x)
  where
    indices = [0 .. length x -1]
    opIndices = [0, next .. length x -1]

restore1202 :: [Int] -> [Int]
restore1202 = restoreWith 12 2

restoreWith :: Int -> Int -> [Int] -> [Int]
restoreWith x y = \case
  a : _ : _ : xs -> a : x : y : xs
  _ -> error "Bad input"

readInput :: FilePath -> IO [Int]
readInput f =
  parseInput f <$> readFileText f

parseInput :: FilePath -> Text -> [Int]
parseInput f =
  either (error . toText . errorBundlePretty) id
    . parse inputParser f

inputParser :: Parser [Int]
inputParser = sepBy1 L.decimal (char ',')
