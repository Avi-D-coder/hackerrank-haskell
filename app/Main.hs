{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Control.Monad

import           Data.Char

import           System.Environment
import           System.IO
import           System.IO.Unsafe

-- Complete the abbreviation function below.
abbreviation :: String -> String -> String
abbreviation a b = if abb a b then "YES" else "NO"

abb :: String -> String -> Bool
abb a  [] = all isLower a
abb [] _  = False
abb a b | ha == hb         = abb ta tb
        | toUpper ha == hb = abb ta tb || abb ta b
        | isUpper ha       = False
        | otherwise        = abb ta b
 where
  ha : ta = a

  hb : tb = b


main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr   <- openFile stdout WriteMode

    q      <- readLn :: IO Int

    forM_ [1 .. q] $ \q_itr -> do
        a <- getLine

        b <- getLine

        let result = abbreviation a b

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr

