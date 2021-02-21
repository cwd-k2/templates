{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecursiveDo        #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Vector.Unboxed        (Vector, (!))
import qualified Data.Vector.Unboxed        as V

rInt = BS.readInt . BS.dropWhile isSpace
lInt = BL.readInt . BL.dropWhile isSpace

rIntT = StateT rInt
lIntT = StateT lInt

rInt2 = runStateT $ (,) <$> rIntT <*> rIntT
lInt2 = runStateT $ (,) <$> lIntT <*> lIntT

rInt3 = runStateT $ (,,) <$> rIntT <*> rIntT <*> rIntT
lInt3 = runStateT $ (,,) <$> lIntT <*> lIntT <*> lIntT

--------------------------------------------------------------------------------

main = do
  return ()

