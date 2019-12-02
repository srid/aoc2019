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
  void $ flip firstM ((,) <$> [0 .. 99] <*> [0 .. 99]) $ \(a, b) ->
    case goHead (restoreWith a b input) of
      -- Expect: (50, 64)
      Just 19690720 -> print (a, b, 100 * a + b) >> pure True
      _ -> pure False
  where
    goHead :: [Int] -> Maybe Int
    goHead = listToMaybe . take 1 . go
    go = fst . runState computer . (0,) . fromList
    sample =
      [ "1,0,0,0,99",
        "2,3,0,3,99",
        "2,4,4,5,99,0",
        "1,1,1,4,99,5,6,0,99"
      ]

type Address = Int
type Memory = Seq Int

computer :: State (Address, Memory) [Int]
computer =
  fmap toList <$> untilJust $ do
    (idx, mem) <- get
    case readAddrMaybe idx mem of
      Nothing -> pure $ Just mem
      Just 99 -> pure $ Just mem
      Just 1 -> put (idx + 4, doOp (+) idx mem) >> pure Nothing
      Just 2 -> put (idx + 4, doOp (*) idx mem) >> pure Nothing
      Just op -> error $ "Bad op code: " <> show op
  where
    readAddr :: Address -> Memory -> Int
    readAddr k = fromMaybe (error "Bad addr") . readAddrMaybe k
    readAddrMaybe = Seq.lookup
    readPointer :: Address -> Memory -> Int
    readPointer k m = fromMaybe (error "Bad pointer") $ readAddrMaybe (readAddr k m) m
    writeMem :: Address -> Int -> Memory -> Memory
    writeMem k v m = Seq.update (readAddr k m) v m
    doOp :: (Int -> Int -> Int) -> Address -> Memory -> Memory
    doOp op idx mem =
      let a = readPointer (idx + 1) mem
          b = readPointer (idx + 2) mem
       in writeMem (idx + 3) (op a b) mem

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
