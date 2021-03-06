{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Util where

------------------------------------------------------------------------------
import           Control.Lens hiding ((<|))
import           Snap.Snaplet
import           System.FilePath
import           Data.Foldable as F
import           Data.Distributive as D
import           Data.Aeson
import           Data.Monoid
import           Data.List
import           Data.Thyme.Clock
import           Data.AffineSpace
import           Data.Bifunctor
import           Data.Bifunctor.Flip
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.ByteString as B
import qualified Data.Traversable as TR
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Stream.Infinite as Z
import qualified Data.Stream.Infinite.Skew as S
import           Data.Stream.Infinite.Skew ((<|))
import           Data.Maybe
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans
import           Control.Error
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Control.Monad.State.Class
import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8

import           Control.Monad.Random
import           System.Random

import           Debug.Trace

------------------------------------------------------------------------------
---- | Converts pure text splices to pure Builder splices.
textSplicesUtf8 :: [(T.Text, a -> T.Text)] -> [(T.Text, a -> Builder)]
textSplicesUtf8 = C.mapSnd textSpliceUtf8

--------------------------------------------------------------------------------
-- | Converts a pure text splice function to a pure Builder splice function.
textSpliceUtf8 :: (a -> T.Text) -> a -> Builder
textSpliceUtf8 f = fromText . f

------------------------------------------------------------------------------

showIntegral :: Integral a => a -> T.Text
showIntegral = toStrict. toLazyText . decimal

------------------------------------------------------------------------------

jsonResponseUtf8 :: MonadSnap m => m ()
jsonResponseUtf8 = 
    modifyResponse $ setHeader "Content-Type" "application/json; charset=utf-8"

------------------------------------------------------------------------------

threadDelay' :: NominalDiffTime -> IO () 
threadDelay' delay = threadDelay $ 
                        delay^.from microNominalDiffTime^.to fromIntegral

------------------------------------------------------------------------------
prepend :: S.Stream a -> [a] -> S.Stream a 
prepend stream = F.foldr (<|) stream 

splitCorrect :: (a -> Bool) -> S.Stream a -> S.Stream a
splitCorrect f stream = 
    let (_,rest) = S.split f stream
        (head',rest') = S.uncons rest
    in if f head' then rest else rest'

------------------------------------------------------------------------------

milis :: Integer -> NominalDiffTime 
milis s = s^.to fromInteger^.to ((*) (10^3))^.microNominalDiffTime


