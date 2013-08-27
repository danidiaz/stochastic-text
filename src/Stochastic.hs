{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Stochastic where

------------------------------------------------------------------------------
import           Control.Lens hiding ((<|))
import           Snap.Snaplet
import           Snap.Extras.JSON
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

import           Util -- out own utils

import           Debug.Trace

------------------------------------------------------------------------------

type Langname = T.Text
type Verse = T.Text

type Multiverse = S.Stream Verse

data Change = Change
    {   _iteration :: Integer
    ,   _diffTime :: NominalDiffTime
    ,   _verseIndex :: Integer
    ,   _languageIndex :: Integer
    }

makeLenses ''Change

data  Eternity = Eternity 
    {   _langCount :: Integer
    ,   _verseCount :: Integer
    ,   _verseStream :: S.Stream Multiverse
    }

makeLenses ''Eternity 

data  Sempiternity = Sempiternity 
    {   _baseTime :: UTCTime  
    ,   _origin :: S.Stream Integer
    ,   _mutations :: S.Stream Change
    }

makeLenses ''Sempiternity 

data StochasticText = StochasticText 
    {   _eternity :: Eternity
    ,   _sempiternity :: MVar Sempiternity
    }

makeLenses ''StochasticText

synch :: MonadIO m => (Eternity -> Sempiternity -> a) 
                   -> StochasticText 
                   -> m a
synch f snaplet = do
    sempiternity' <- liftIO . readMVar $ snaplet^.sempiternity 
    return $ f (snaplet^.eternity) sempiternity' 

currentPoem :: UTCTime 
            -> Eternity 
            -> Sempiternity 
            -> (Integer,T.Text,[(Integer,T.Text)])
currentPoem time eternity' sempiternity' =
    let sempiternity'' =  purgePast time sempiternity'
        headChange = S.head (sempiternity''^.mutations)
        verses = S.index <$> (sempiternity''^.origin) <*> (eternity'^.verseStream) 
        ((_,title) : renderedVerses) = -- index 0 reserved for title
            genericTake (eternity'^.verseCount) . F.toList . S.indexed $ verses 
    in (headChange^.iteration, title, renderedVerses)

readChangeBatch :: Integer 
                -> Integer 
                -> Eternity 
                -> Sempiternity 
                -> [(Integer,Integer,T.Text)]  
readChangeBatch limit count eternity' sempiternity' = 
    let (_,rest) = sempiternity'^.mutations
                                ^.to (S.split $ (<=) limit . view iteration)
        triplet c = ( 
                      c^.diffTime^.from microNominalDiffTime
                                 ^.to (flip div 1000)
                                 ^.to fromIntegral
                    , c^.verseIndex
                    , eternity'^.verseStream^.to (c^.verseIndex^.to S.index)
                                            ^.to (c^.languageIndex^.to S.index)
                    )
    in genericTake count . F.toList $ fmap triplet rest 

calcTimes :: UTCTime -> S.Stream Change -> S.Stream (Change,UTCTime)  
calcTimes time changes = 
    let go :: UTCTime -> Change -> (UTCTime,(Change,UTCTime))   
        go base change = 
            let newtime = base .+^ (change ^. diffTime)
            in (newtime,(change,newtime)) 
    in snd $ TR.mapAccumL go time changes

splitByTime :: UTCTime -> S.Stream (Change,UTCTime) -> ([Change],S.Stream Change)
splitByTime time stream = 
    let (prefix,stream') = S.split ( (<) time . snd ) stream
    in (fmap fst prefix, fmap fst stream') 

applyChanges :: [Change] -> S.Stream Integer -> S.Stream Integer
applyChanges changes stream = 
    let go stream change = S.adjust (change^.verseIndex) 
                                    (change^.languageIndex^.to const) 
                                    stream
    in F.foldl' go stream changes 

purgePast :: UTCTime -> Sempiternity -> Sempiternity 
purgePast time sempiternity = 
    let (changes,stream) =  splitByTime time
                          $ sempiternity^.baseTime^.to calcTimes 
                          $ sempiternity^.mutations   
        newBase = applyChanges changes (sempiternity^.origin)
    in Sempiternity time newBase stream 

------------------------------------------------------------------------------

compile :: (S.Stream (S.Stream a)) -> 
           [[a]] -> 
           S.Stream (S.Stream a)
compile filler xss = prepend <$> filler <*> prepend (S.repeat []) xss 

------------------------------------------------------------------------------

futurify :: (Applicative m, MonadRandom m) 
          => Eternity 
          -> m (S.Stream Integer, S.Stream Change)
futurify eternity = do 
    let vc = eternity^.verseCount 
        lc = eternity^.langCount 
    initialState <- fmap (prepend (S.repeat 0)) $ 
        TR.sequence . replicate (fromIntegral vc) $ getRandomR (0, lc - 1) 
    randoms <- TR.sequence . S.repeat $ 
                    (,) <$> getRandomR (0, vc - 1) 
                        -- number of alternatives to a given verse is lc - 1 
                        <*> getRandomR (0, lc - 2) 
    let updateStream stream (vi,li) = 
            let li' = if stream S.!! vi >= li then li + 1 else li
            in (S.update vi li' stream, (vi,li'))
        changes = ((fmap (fmap uncurry)) Change) 
                    <$> S.tabulate id
                    <*> S.repeat (milis 1000) 
                    <*> (snd $ TR.mapAccumL updateStream initialState randoms)
    return (initialState,changes)


--futurify :: StdGen -> Eternity -> S.Stream Change
--futurify seed e = 
--    let (seed',seed'') = runRand getSplit seed
--    in Change <$> S.tabulate id
--              <*> S.repeat (milis 1000)
--              <*> ristream (e^.verseCount) seed'
--              -- The number of alternatives to a given verse.
--              <*> ristream (e^.langCount - 1) seed'' 

------------------------------------------------------------------------------

currentPoemH :: SnapletLens b StochasticText -> 
                Handler b b (Integer,T.Text,[(Integer,T.Text)])
currentPoemH lens = withTop lens $ do
    time <- liftIO getCurrentTime 
    liftIO . synch (currentPoem time) =<< get

poemSplice :: Monad n => RuntimeSplice n (Integer,T.Text,[(Integer,T.Text)]) -> 
                         C.Splice n
poemSplice = C.withSplices C.runChildren splicefuncs 
    where
    splicefuncs = 
        [ ("iteration", C.pureSplice . textSpliceUtf8 $ showIntegral . (^._1) )
        , ("poemtitle", C.pureSplice . textSpliceUtf8 $ (^._2) )
        , ("verses",  verseSplice . liftM (^._3) . C.getPromise) 
        ]

verseSplice :: Monad n => RuntimeSplice n [(Integer,T.Text)] -> 
                          C.Splice n
verseSplice = C.manyWithSplices C.runChildren splicefuncs 
    where
    splicefuncs = C.pureSplices . textSplicesUtf8 $ 
        [ ("verseid", showIntegral . (^._1))
        , ("verse", (^._2)) 
        ]

------------------------------------------------------------------------------

addPoemSplices :: HasHeist b => Snaplet (Heist b) -> 
                                SnapletLens b StochasticText -> 
                                Initializer b v ()
addPoemSplices h poem = addConfig h $ mempty 
    {
        hcCompiledSplices = [("poem", poemSplice . lift $ currentPoemH poem)] 
    } 

------------------------------------------------------------------------------

langolier :: NominalDiffTime -> NominalDiffTime -> MVar Sempiternity -> IO ()
langolier delay distanceToPast sempiternity' = do
    threadDelay' distanceToPast -- give time for the past to form
    forever $ do
        threadDelay' delay
        now <- liftIO getCurrentTime  
        let newPast = now .-^ distanceToPast  
        modifyMVar_ sempiternity' $ return . purgePast newPast 


initVerses :: SnapletInit b StochasticText
initVerses  = do
    makeSnaplet "stochastic" "Provider of stochastic text" Nothing $ do
        path <- flip combine "sample_poem.js" <$> getSnapletFilePath
        printInfo $ "Loading poem from: " <> T.pack path
        versebytes <- liftIO $ BL.fromChunks . pure <$> B.readFile path
        let mapE = first (mappend "Error loading poem: " . T.pack)
                         (eitherDecode' versebytes)
        TR.traverse printInfo $ Flip mapE
        seed <- liftIO newStdGen 
        now <- liftIO getCurrentTime 
        let poemz = maybe M.empty id $ hush mapE :: M.Map Langname [Verse]
            elemz = M.elems poemz -- discard the language names
            langCount' = genericLength elemz
            verseCount' = F.maximum . map genericLength $ elemz
            filler = S.repeat . S.repeat $ "buffalo" 
            verses = distribute $ compile filler elemz         
            eternity' = Eternity langCount' verseCount' verses
            (origin',mutations') = evalRand (futurify eternity') seed
        snaplet <- StochasticText eternity' <$>
                       (liftIO . newMVar $ Sempiternity now origin' mutations')
        liftIO . forkIO $ langolier (milis 10000) 
                                    (milis 30000)   
                                    (snaplet^.sempiternity) 
        return snaplet 

------------------------------------------------------------------------------


