{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
-- import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the countingValleys function below.
countingValleys:: Int -> String -> Int
countingValleys n = fst . foldl' newAlt (0, 0)
    where
        newAlt (val, alt) step
            | alt == -1 && step == 'U' = (val + 1, 0)
            | step == 'D' = (val, alt - 1)
            | step == 'U' = (val, alt + 1)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    s <- getLine

    let result = countingValleys n s

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
