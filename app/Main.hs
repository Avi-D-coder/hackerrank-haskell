{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -O2 #-}

module Main where

import           Control.Monad
import           Control.Monad.ST

import           Data.Char
import           Data.HashTable.ST.Basic       as H
import           Data.STRef
import qualified Data.Text                     as T

import           System.Environment
import           System.IO

-- Complete the abbreviation function below.
abbreviation :: String -> String -> String
abbreviation a b = if abbM (T.pack a) (T.pack b) then "YES" else "NO"

abbM :: T.Text -> T.Text -> Bool
abbM a b = runST $ do
  m <- H.newSized 100000
  abb a b m

abb :: T.Text -> T.Text -> HashTable s (T.Text, T.Text) Bool -> ST s Bool
abb a  "" _ = return $ T.all isLower a
abb "" _  _ = return False
abb a  b  m = do
  l <- H.lookup m (a, b)
  case l of
    Just memo -> return memo
    Nothing   -> do
      r <- recur
      H.insert m (a, b) r
      recur
 where
  ha = T.head a

  ta = T.tail a

  hb = T.head b

  tb = T.tail b

  recur
    | T.length a < T.length b = return False
    | ha == hb = abb ta tb m
    | toUpper ha == hb = do
      rm <- abb ta b m
      uc <- abb ta tb m
      return $ rm || uc
    | isUpper ha = return False
    | otherwise = abb ta b m

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
