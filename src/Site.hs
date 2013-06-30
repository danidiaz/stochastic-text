{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State.Class
import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application

------------------------------------------------------------------------------
-- | 
handlePoem :: Handler App (StochasticText) ()
handlePoem = cRender "_poem"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/poem",     with poem handlePoem)
         , ("",          serveDirectory "static")
         ]

verseSplice :: forall b. SnapletLens b StochasticText ->  C.Splice (Handler b b)
verseSplice lens = 
    let splicemap :: Monad n => [(T.Text, C.Promise T.Text -> C.Splice n)]
        splicemap =  C.pureSplices . C.textSplices $ [("versetext",id)]

        vs :: RuntimeSplice (Handler b b) [T.Text]
        vs = lift . withTop lens $ use verses
    in C.manyWithSplices C.runChildren splicemap vs

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    p <- nestSnaplet "poem" poem $ 
            makeSnaplet "StochasticText" 
                        "Provider of stochastic text"
                        Nothing 
                        (return . StochasticText $ ["first verse","second verse"])
    addConfig h $ mempty {
            hcCompiledSplices = [ ("verse", verseSplice poem)  ] 
        } 
    addRoutes routes
    return $ App h p

