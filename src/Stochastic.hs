{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Stochastic where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet

import           System.FilePath
import           Data.Aeson
import           Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.ByteString as B
import qualified Data.Traversable as TR
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Data.Maybe
import           Control.Applicative
import           Control.Concurrent
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
------------------------------------------------------------------------------
data StochasticText = StochasticText 
    { _verses :: [T.Text]
    }

makeLenses ''StochasticText

------------------------------------------------------------------------------

------------------------------------------------------------------------------
---- | Converts pure text splices to pure Builder splices.
textSplicesUtf8 :: [(T.Text, a -> T.Text)] -> [(T.Text, a -> Builder)]
textSplicesUtf8 = C.mapSnd textSpliceUtf8


--------------------------------------------------------------------------------
-- | Converts a pure text splice function to a pure Builder splice function.
textSpliceUtf8 :: (a -> T.Text) -> a -> Builder
textSpliceUtf8 f = fromText . f

------------------------------------------------------------------------------
verseSplice :: forall b. SnapletLens b StochasticText ->  C.Splice (Handler b b)
verseSplice lens = 
    let splicemap :: Monad n => [(T.Text, C.Promise T.Text -> C.Splice n)]
        splicemap =  C.pureSplices . textSplicesUtf8 $ [("versetext",id)]

        vs :: RuntimeSplice (Handler b b) [T.Text]
        vs = lift . withTop lens $ use verses
    in C.manyWithSplices C.runChildren splicemap vs

------------------------------------------------------------------------------
addVerseSplices :: HasHeist b => Snaplet (Heist b) 
                                   -> SnapletLens b StochasticText
                                   -> Initializer b v ()
addVerseSplices h poem = addConfig h $ mempty 
        {
            hcCompiledSplices = [ ("verse", verseSplice poem)  ] 
        } 

------------------------------------------------------------------------------

type Langname = T.Text
type Verse = T.Text

instance Monoid a => Monoid (ZipList a) where
    mappend = liftA2 mappend
    mempty = ZipList $ repeat mempty

compile :: M.Map Langname [Verse] -> (I.IntMap Langname, Int, [I.IntMap Verse])
compile m = (I.fromList pairs, length multiverse, multiverse)
    where pairs =  zip [0..] $ M.keys m
          monoidal (number,name) = 
                ZipList . map (I.singleton number) <$> M.lookup name m
          multiverse = getZipList . mconcat . catMaybes . map monoidal $ pairs

------------------------------------------------------------------------------

initVerses :: SnapletInit b StochasticText
initVerses  = do
    makeSnaplet "stochastic" "Provider of stochastic text" Nothing $ do
        path <- flip combine "sample_poem.js" <$> getSnapletFilePath
        printInfo $ "Loading poem from: " <> T.pack path
        versebytes <- liftIO $ BL.fromChunks . pure <$> B.readFile path
        let versesE = fmapL ("Error loading poem:"<>) $ do 
                versions <- fmapL T.pack $ eitherDecode' versebytes
                note "Language not found" $ M.lookup ("french"::T.Text) versions
            (verses,msg) = case versesE of
                Left err -> ( ["buffalo"], Just err )
                Right v ->  ( v, Nothing ) 
        TR.traverse printInfo msg
        liftIO . forkIO . forever $  
            threadDelay 1000000 >> putStrLn "This is a refresh action."
        return $ StochasticText verses

------------------------------------------------------------------------------


