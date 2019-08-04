{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -O2 #-}

module Main where

import           Control.Monad
import           Control.Monad.ST

import qualified Data.ByteString.Char8         as C
import           Data.Char
import qualified Data.HashTable.ST.Basic       as H

-- Complete the abbreviation function below.
abbreviation :: C.ByteString -> C.ByteString -> C.ByteString
abbreviation a b = if abbM a b then "YES" else "NO"

abbM :: C.ByteString -> C.ByteString -> Bool
abbM a b = runST $ do
  m <- H.newSized 100000
  abb a b m

abb
  :: C.ByteString
  -> C.ByteString
  -> H.HashTable s (C.ByteString, C.ByteString) Bool
  -> ST s Bool
abb a  "" _ = return $! C.all isLower a
abb "" _  _ = return False
abb a  b  m = do
  l <- H.lookup m (a, b)
  case l of
    Just memo -> return memo
    Nothing   -> do
      r <- recur
      H.insert m (a, b) r
      return r
 where
  Just (ha, ta) = C.uncons a

  Just (hb, tb) = C.uncons b

  recur
    | C.length a < C.length b = return False
    | ha == hb = abb ta tb m
    | toUpper ha == hb = do
      rm <- abb ta b m
      if rm then return True else abb ta tb m
    | isUpper ha = return False
    | otherwise = abb ta b m

main :: IO ()
main = do
  q <- getLine
  replicateM_ (read q) question

question :: IO ()
question = do
  a <- C.getLine
  b <- C.getLine
  C.putStrLn $! abbreviation a b
