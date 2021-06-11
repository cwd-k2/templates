{-# OPTIONS_GHC -XBangPatterns  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecursiveDo        #-}
{-# LANGUAGE TupleSections      #-}

import           Prelude                    hiding (getContents)

import           Control.Applicative        ((<|>))
import           Control.Monad
import           Control.Monad.State        (StateT (StateT), evalStateT)
import           Data.Char                  (isDigit, isSpace)
import           Data.Function
import           Data.Maybe                 (fromJust)
-- 高速化
import           Data.ByteString.Lazy.Char8 (ByteString, getContents)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Vector.Unboxed        (Vector, (!))
import qualified Data.Vector.Unboxed        as V
-- C FFI (for atof)
import qualified Data.ByteString            as B (useAsCString)
import           Foreign.C                  (CDouble (CDouble), CString)
import           System.IO.Unsafe           (unsafePerformIO)

foreign import ccall unsafe "static stdlib.h atof"
  c_atof :: CString -> IO CDouble

atof :: ByteString -> Double
atof b = unsafePerformIO . B.useAsCString (B.toStrict b) $
  fmap realToFrac . c_atof
-- 入力パーサの準備
type Input = StateT ByteString Maybe

char :: Char -> Input Char
char !c = StateT $ \str ->
  if B.null str || c /= B.head str
     then Nothing
     else Just (c, B.tail str)

spaces :: Input ()
spaces = StateT $ Just . ((), ) . B.dropWhile isSpace

getUntil :: (Char -> Bool) -> Input ByteString
getUntil p = StateT $ \str ->
  let (f, s) = B.break p str
   in if B.null f then Nothing else Just (f, s)

rInt :: Input Int
rInt = spaces *> StateT B.readInt

rDbl :: Input Double
rDbl = atof <$> (spaces *> (dotted <|> digits))
  where digits = getUntil (not . isDigit)
        dotted = (<>) . (<> ".") <$> digits <*> (char '.' *> digits)
-- 以下その場で定義
input :: Input ()
input = do
  return ()

main :: IO ()
main = do
  () <- fromJust . evalStateT input <$> getContents
  return ()

