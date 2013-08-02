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
import           Data.Bifunctor
import           Data.Bifunctor.Flip
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.ByteString as B
import qualified Data.Traversable as TR
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Stream.Infinite.Skew as S
import           Data.Stream.Infinite.Skew ((<|))
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

prepend :: S.Stream a -> [a] -> S.Stream a 
prepend stream = F.foldr (<|) stream 

compile :: (S.Stream (S.Stream a)) -> 
           [[a]] -> 
           (Int, Int, S.Stream (S.Stream a))
compile filler xss = (length xss, F.maximum . map length $ xss, grid)
    where 
        grid = prepend <$> filler <*> prepend (S.repeat []) xss 

type Langname = T.Text
type Verse = T.Text

------------------------------------------------------------------------------

initVerses :: SnapletInit b StochasticText
initVerses  = do
    makeSnaplet "stochastic" "Provider of stochastic text" Nothing $ do
        path <- flip combine "sample_poem.js" <$> getSnapletFilePath
        printInfo $ "Loading poem from: " <> T.pack path
        versebytes <- liftIO $ BL.fromChunks . pure <$> B.readFile path
        let filler = S.repeat . S.repeat $ "buffalo" 

            decoded :: Either String (M.Map Langname [Verse])
            decoded = eitherDecode' versebytes
            versesE = bimap (mappend "Error loading poem: " . T.pack)
                            (compile filler . M.elems)
                            decoded

            fallback = (1, 1, filler)
            (langCount,verseCount,poems) = maybe fallback id (hush versesE)
            verses = distribute poems
        TR.traverse printInfo $ Flip versesE
        liftIO . forkIO . forever $  
            threadDelay 1000000 >> putStrLn "This is a refresh action."
        return . StochasticText $ 
            take verseCount . F.toList . fmap S.head $ verses

------------------------------------------------------------------------------


