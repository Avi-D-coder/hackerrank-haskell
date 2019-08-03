{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.ST

import           Data.Char
import qualified Data.Map.Strict               as M
import           Data.STRef
import qualified Data.Text                     as T

import           System.Environment
import           System.IO
import           System.IO.Unsafe

-- Complete the abbreviation function below.
abbreviation :: String -> String -> String
abbreviation a b = if abbM (T.pack a) (T.pack b) then "YES" else "NO"

abbM :: T.Text -> T.Text -> Bool
abbM a b = runST $ do
  m <- newSTRef M.empty
  abb a b m

abb :: T.Text -> T.Text -> STRef s (M.Map (T.Text, T.Text) Bool) -> ST s Bool
abb a  "" _ = return $ T.all isLower a
abb "" _  _ = return False
abb a  b  m = do
  mm <- readSTRef m
  case M.lookup (a, b) mm of
    Just memo -> return memo
    Nothing   -> do
      r <- recur
      modifySTRef m $ M.insert (a, b) r
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
