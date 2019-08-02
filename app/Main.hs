{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Control.Monad

import           Data.Array
import           Data.Bits

-- import Data.List
-- import Data.List.Split
-- import Data.Set
-- import Debug.Trace
import           System.Environment
import           System.IO
import           System.IO.Unsafe

-- -2 3 2 4 5
-- Complete the maxSubsetSum function below.
maxSubsetSum :: [Int] -> Int
maxSubsetSum = uncurry max . foldr f (0, 0)

f :: Int -> (Int, Int) -> (Int, Int)
f n (p', p) = (max p' p, max n $ p' + n)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray (n - 1)
    return (line : rest)

main :: IO ()
main = do
    stdout  <- getEnv "OUTPUT_PATH"
    fptr    <- openFile stdout WriteMode

    n       <- readLn :: IO Int

    arrTemp <- getLine

    let arr = map (read :: String -> Int) . words $ arrTemp

    let res = maxSubsetSum arr

    hPutStrLn fptr $ show res

    hFlush fptr
    hClose fptr
