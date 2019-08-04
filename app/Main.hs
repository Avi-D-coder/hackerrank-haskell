{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Arrow                  ( first )
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

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap . first) f . enumerate

instance HasTrie T.Text where
  newtype (T.Text :->: b) = TextTrie (String :->: b)

  trie f = TextTrie (trie (f . T.pack))

  untrie (TextTrie t) = untrie t . T.unpack

  enumerate (TextTrie t) = enum' T.pack t

-- Complete the abbreviation function below.
abbreviation :: String -> String -> String
abbreviation a b = if abb (T.pack a) (T.pack b) then "YES" else "NO"

abb :: T.Text -> T.Text -> Bool
abb a  "" = T.all isLower a
abb "" _  = False
abb a b | ha == hb         = memo2 abb ta tb
        | toUpper ha == hb = memo2 abb ta tb || memo2 abb ta b
        | isUpper ha       = False
        | otherwise        = memo2 abb ta b
 where
  ha = T.head a

  ta = T.tail a

  hb = T.head b

  tb = T.tail b

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
