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

------------------------------------------------------------------------------

type Langname = T.Text
type Verse = T.Text

type Multiverse = S.Stream Verse

asecond :: NominalDiffTime 
asecond = (10^6)^.microNominalDiffTime 

data Change = Change
    {   _iteration :: Integer,
        _diffTime :: NominalDiffTime,
        _verseIndex :: Integer,
        _languageIndex :: Integer
    }

makeLenses ''Change

data  Sempiternity = Sempiternity 
    {   _baseTime :: UTCTime,  
        _origin :: S.Stream Integer,
        _mutations :: S.Stream Change
    }

makeLenses ''Sempiternity 

data StochasticText = StochasticText 
    {   _langCount :: Integer,
        _verseCount :: Integer,
        _verseStream :: S.Stream Multiverse,
        _sempiternity :: MVar Sempiternity
    }

makeLenses ''StochasticText

present :: MonadIO m => StochasticText -> m [(Integer,T.Text)]  
present snaplet = do
    time <- liftIO getCurrentTime 
    sempiternity' <- liftIO . readMVar $ snaplet^.sempiternity 
    let sempiternity'' =  purgePast time sempiternity'
        vs = S.index <$> (sempiternity''^.origin) <*> (snaplet^.verseStream) 
    return $ genericTake (snaplet^.verseCount) . F.toList . S.indexed $ vs 

calctimes :: UTCTime -> S.Stream Change -> S.Stream (Change,UTCTime)  
calctimes time changes = 
    let go :: UTCTime -> Change -> (UTCTime,(Change,UTCTime))   
        go base change = 
            let newtime = base .+^ (change ^. diffTime)
            in (newtime,(change,newtime)) 
    in snd $ TR.mapAccumL go time changes

splitByTime :: UTCTime -> S.Stream (Change,UTCTime) -> ([Change],S.Stream Change)
splitByTime time stream = 
    let (prefix,stream') = S.split ( (time<) . snd ) stream
    in (fmap fst prefix, fmap fst stream') 

applyChanges :: [Change] -> S.Stream Integer -> S.Stream Integer
applyChanges changes stream = 
    let go stream change = S.adjust (change^.verseIndex) 
                                    (const $ change^.languageIndex) 
                                    stream
    in F.foldl' go stream changes 

purgePast :: UTCTime -> Sempiternity -> Sempiternity 
purgePast time sempiternity = 
    let (changes,stream) =  splitByTime time
                          . calctimes (sempiternity^.baseTime) 
                          $ sempiternity^.mutations   
        newBase = applyChanges changes (sempiternity^.origin)
    in Sempiternity time newBase stream 

------------------------------------------------------------------------------

prepend :: S.Stream a -> [a] -> S.Stream a 
prepend stream = F.foldr (<|) stream 

compile :: (S.Stream (S.Stream a)) -> 
           [[a]] -> 
           S.Stream (S.Stream a)
compile filler xss = prepend <$> filler <*> prepend (S.repeat []) xss 

------------------------------------------------------------------------------

ristream :: Integer -> StdGen -> S.Stream Integer    
ristream bound seed = flip evalRand seed $ do
    TR.sequence . S.repeat $ getRandomR (0, pred bound) 

futurify :: StdGen -> Integer -> Integer -> S.Stream Change
futurify seed langCount' verseCount' = 
    let (s',s'') = runRand getSplit seed
    in Change <$> S.tabulate id
              <*> S.repeat asecond
              <*> ristream verseCount' s'
              <*> ristream langCount' s''

------------------------------------------------------------------------------
---- | Converts pure text splices to pure Builder splices.
textSplicesUtf8 :: [(T.Text, a -> T.Text)] -> [(T.Text, a -> Builder)]
textSplicesUtf8 = C.mapSnd textSpliceUtf8

--------------------------------------------------------------------------------
-- | Converts a pure text splice function to a pure Builder splice function.
textSpliceUtf8 :: (a -> T.Text) -> a -> Builder
textSpliceUtf8 f = fromText . f

showIntegral :: Integral a => a -> T.Text
showIntegral = toStrict. toLazyText . decimal

------------------------------------------------------------------------------
verseSplice :: forall b. SnapletLens b StochasticText ->  C.Splice (Handler b b)
verseSplice lens = 
    let splicemap :: Monad n => [(T.Text, C.Promise (Integer,T.Text) -> C.Splice n)]
        splicemap =  C.pureSplices . textSplicesUtf8 $ 
                        [ ("verse", snd), 
                          ("verseid", showIntegral . fst ) ]

        vs :: RuntimeSplice (Handler b b) [(Integer,T.Text)]
        vs = lift . withTop lens $ get >>= liftIO . present
    in C.manyWithSplices C.runChildren splicemap vs

------------------------------------------------------------------------------

addVerseSplices :: HasHeist b => Snaplet (Heist b) 
                                   -> SnapletLens b StochasticText
                                   -> Initializer b v ()
addVerseSplices h poem = addConfig h $ mempty 
        {
            hcCompiledSplices = [ ("verses", verseSplice poem)  ] 
        } 

------------------------------------------------------------------------------

initVerses :: SnapletInit b StochasticText
initVerses  = do
    makeSnaplet "stochastic" "Provider of stochastic text" Nothing $ do
        path <- flip combine "sample_poem.js" <$> getSnapletFilePath
        printInfo $ "Loading poem from: " <> T.pack path
        versebytes <- liftIO $ BL.fromChunks . pure <$> B.readFile path
        let mapE = first (mappend "Error loading poem: " . T.pack)
                         (eitherDecode' versebytes)
        TR.traverse printInfo $ Flip mapE
        (s',s'') <- liftIO $ (,) <$> newStdGen <*> newStdGen  
        now <- liftIO getCurrentTime 
        let poemz = maybe M.empty id $ hush mapE :: M.Map Langname [Verse]
            elemz = M.elems poemz -- discard the language names
            langCount' = genericLength elemz
            verseCount' = F.maximum . map genericLength $ elemz
            filler = S.repeat . S.repeat $ "buffalo" 
            verses = distribute $ compile filler elemz         
            origin' = ristream langCount' s'
            mutations' = futurify s'' langCount' verseCount' 
        snaplet <- StochasticText langCount' verseCount' verses <$>
                       (liftIO . newMVar $ Sempiternity now origin' mutations')
        liftIO . forkIO . forever $  
            threadDelay 1000000 >> putStrLn "This is a refresh action."
        return snaplet 

------------------------------------------------------------------------------


