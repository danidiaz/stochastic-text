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
import           Data.Text
import           Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
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
    { _verses :: [Text]
    }

makeLenses ''StochasticText

------------------------------------------------------------------------------

------------------------------------------------------------------------------
---- | Converts pure text splices to pure Builder splices.
textSplicesUtf8 :: [(Text, a -> Text)] -> [(Text, a -> Builder)]
textSplicesUtf8 = C.mapSnd textSpliceUtf8


--------------------------------------------------------------------------------
-- | Converts a pure text splice function to a pure Builder splice function.
textSpliceUtf8 :: (a -> Text) -> a -> Builder
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
initVerses :: SnapletInit b StochasticText
initVerses  = do
    makeSnaplet "stochastic" "Provider of stochastic text" Nothing $ do
        path <- flip combine "sample_poem.js" <$> getSnapletFilePath
        printInfo $ "Loading poem from: " <> T.pack path
        versebytes <- liftIO . B.readFile $ path
        let versesEither = eitherDecode' . BL.fromChunks $ [ versebytes ]
        case versesEither of
            Left err -> printInfo $ "Error loading poem: " <> T.pack err
            Right _ -> return () 
        let langolier = lift . forkIO $ forever $  
                threadDelay 1000000 >> putStrLn "This is a refresh action."
        addPostInitHook $ \x -> langolier >> return x 
        let verses = either (const ["buffalo"]) id versesEither
        return $ StochasticText verses

------------------------------------------------------------------------------


