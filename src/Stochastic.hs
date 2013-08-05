{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Stochastic where

------------------------------------------------------------------------------
import Control.Lens hiding ((<|))
import Snap.Snaplet

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

------------------------------------------------------------------------------

type Langname = T.Text
type Verse = T.Text

type Multiverse = S.Stream Verse

asecond :: DiffTime 
asecond = (10^6)^.microDiffTime 

data  Sempiternity = Sempiternity 
    {   
        _baseTime :: UTCTime,  
        _iteration :: Integer,
        _origin :: S.Stream Integer,
        -- Verse id, lang id. Assume each mutation last a second.
        _mutations :: S.Stream (Integer,Integer) 
    }

makeLenses ''Sempiternity 

data StochasticText = StochasticText 
    {   _langCount :: Integer,
        _verseCount :: Integer,
        _verseStream :: S.Stream Multiverse,

        _sempiternity :: MVar Sempiternity
    }

makeLenses ''StochasticText

present :: MonadIO m => StochasticText -> m [T.Text]  
present st = do
    sempiternity' <- liftIO . readMVar $ st ^. sempiternity 
    let vs = S.index <$> (sempiternity'^.origin) <*> (st^.verseStream) 
    return $ genericTake (st ^. verseCount) . F.toList $ vs 


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
        vs = lift . withTop lens $ get >>= present
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

prepend :: S.Stream a -> [a] -> S.Stream a 
prepend stream = F.foldr (<|) stream 

compile :: (S.Stream (S.Stream a)) -> 
           [[a]] -> 
           S.Stream (S.Stream a)
compile filler xss = prepend <$> filler <*> prepend (S.repeat []) xss 

------------------------------------------------------------------------------

futurify :: StdGen -> 
            (Integer,Integer) -> 
            (S.Stream Integer, S.Stream (Integer,Integer))
futurify seed (langCount',verseCount') = flip evalRand seed $ do
    seed' <- getSplit :: Rand StdGen StdGen
    let rLangIndex = getRandomR (0, pred langCount') 
        rVerseIndex = getRandomR (0, pred verseCount') 
        mutations = flip evalRand seed' $ do
            TR.sequence . S.repeat $ (,) <$> rVerseIndex <*> rLangIndex
    (,) <$> (TR.sequence . S.repeat $ rLangIndex) <*> return mutations

------------------------------------------------------------------------------

initVerses :: SnapletInit b StochasticText
initVerses  = do
    makeSnaplet "stochastic" "Provider of stochastic text" Nothing $ do
        path <- flip combine "sample_poem.js" <$> getSnapletFilePath
        printInfo $ "Loading poem from: " <> T.pack path
        versebytes <- liftIO $ BL.fromChunks . pure <$> B.readFile path
        let mapE = bimap (mappend "Error loading poem: " . T.pack)
                         id 
                         (eitherDecode' versebytes)
        TR.traverse printInfo $ Flip mapE
        let poemz = maybe M.empty id $ hush mapE :: M.Map Langname [Verse]
            elemz = M.elems poemz -- discard the language names
            langCount' = genericLength elemz
            verseCount' = F.maximum . map genericLength $ elemz
            filler = S.repeat . S.repeat $ "buffalo" 
            verses = distribute $ compile filler elemz         
        stdgen <- liftIO getStdGen 
        let (origin',mutations) = futurify stdgen (langCount',verseCount')
        now <- liftIO getCurrentTime 
        snaplet <- StochasticText langCount' verseCount' verses <$>
                       (liftIO . newMVar $ Sempiternity now 0 origin' mutations)
        liftIO . forkIO . forever $  
            threadDelay 1000000 >> putStrLn "This is a refresh action."
        return snaplet 

------------------------------------------------------------------------------


