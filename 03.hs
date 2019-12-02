#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "import ./haskell.nix (p: [p.relude p.megaparsec p.monad-loops])"
#! nix-shell --pure -i "ghcid --warnings -T main"

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Monad.Loops (iterateWhile)
import Relude
import Relude.Extra.Map
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

main :: IO ()
main = do
  forM_ sample $ \s -> do
    putStr "Trying ... "
    print s
    print $ go $ parseInput ".." s
  print . go . restore1202 =<< readInput "input/2"
  where
    go = fst . runState computer . indexList 4
    sample =
      [ "1,0,0,0,99",
        "2,3,0,3,99",
        "2,4,4,5,99,0",
        "1,1,1,4,99,5,6,0,99"
      ]

computer :: State ([Int], Map Int Int) [Int]
computer = do
  iterateWhile id $ do
    (indices, nums) <- get
    case indices of
      [] -> pure False
      idx : is -> case (readAddr idx nums) of
        Just 99 -> pure False
        Just 1 -> put (is, doOp (+) idx nums) >> pure True
        Just 2 -> put (is, doOp (*) idx nums) >> pure True
        Just op -> error $ "Bad op code: " <> show op
        Nothing -> error "Bad op ref"
  elems . snd <$> get
  where
    readAddr :: Int -> Map Int Int -> Maybe Int
    readAddr = lookup
    readMem :: Int -> Map Int Int -> Maybe Int
    readMem k m = do
      addr <- lookup k m
      lookup addr m
    doOp :: (Int -> Int -> Int) -> Int -> Map Int Int -> Map Int Int
    doOp op idx nums = case ((,,) <$> readMem (idx + 1) nums <*> readMem (idx + 2) nums <*> readAddr (idx + 3) nums) of
      Just (a, b, c) ->
        alter (maybe (error "Bad memory ref") (const $ Just $ op a b)) c nums
      Nothing -> error "Bad memory ref"

indexList :: Int -> [a] -> ([Int], Map Int a)
indexList next x = (opIndices, fromList $ zip indices x)
  where
    indices = [0 .. length x -1]
    opIndices = [0, next .. length x -1]

restore1202 :: [Int] -> [Int]
restore1202 = \case
  a : _ : _ : xs -> a : 12 : 2 : xs
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
