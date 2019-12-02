#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude p.megaparsec p.monad-loops])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Monad.Loops (firstM, untilJust)
import qualified Data.Sequence as Seq
import Relude
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

main :: IO ()
main = do
  input <- readInput "input/2"
  -- Expect 5098658
  print $ go $ restoreWith 12 2 input
  void $ flip firstM ((,) <$> [0 .. 99] <*> [0 .. 99]) $ \(a, b) ->
    case go (restoreWith a b input) of
      -- Expect: (50, 64)
      Just 19690720 -> print (a, b, 100 * a + b) >> pure True
      _ -> pure False
  where
    go :: [Int] -> Maybe Int
    go = listToMaybe . take 1 . fst . runState computer . (0,) . fromList

type Address = Int

type Memory = Seq Int

computer :: State (Address, Memory) [Int]
computer =
  fmap toList <$> untilJust $ do
    (idx, mem) <- get
    case readAddrMaybe idx mem of
      Nothing -> pure $ Just mem
      Just 99 -> pure $ Just mem
      Just 1 -> put (doOp (+) idx mem) >> pure Nothing
      Just 2 -> put (doOp (*) idx mem) >> pure Nothing
      Just op -> error $ "Bad op code: " <> show op
  where
    readAddr :: Address -> Memory -> Int
    readAddr k v =
      readAddrMaybe k v
        ?: error "Bad addr"
    readAddrMaybe = Seq.lookup
    readPointer :: Address -> Memory -> Int
    readPointer k m =
      readAddrMaybe (readAddr k m) m
        ?: error "Bad pointer"
    writePointerVal :: Address -> Int -> Memory -> Memory
    writePointerVal k v m = Seq.update (readAddr k m) v m
    doOp :: (Int -> Int -> Int) -> Address -> Memory -> (Address, Memory)
    doOp op idx mem =
      let a = readPointer (idx + 1) mem
          b = readPointer (idx + 2) mem
       in (idx + 4, writePointerVal (idx + 3) (op a b) mem)

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
