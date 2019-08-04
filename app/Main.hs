{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad

import           Data.Char
import qualified Data.Map.Strict               as M
import           Data.MemoTrie
import           Data.STRef
import qualified Data.Text                     as T

import           GHC.Generics                   ( Generic )

import           System.Environment
import           System.IO
import           System.IO.Unsafe

-- Complete the abbreviation function below.
abbreviation :: String -> String -> String
abbreviation a b = if abb a b then "YES" else "NO"

abb :: String -> String -> Bool
abb a  [] = all isLower a
abb [] _  = False
abb a b | ha == hb         = memo2 abb ta tb
        | toUpper ha == hb = memo2 abb ta tb || memo2 abb ta b
        | isUpper ha       = False
        | otherwise        = memo2 abb ta b
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

