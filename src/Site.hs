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
import           Data.Aeson
import           Data.Monoid
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Read
import           Data.Text.Encoding
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Snap.Extras.JSON
import           Stochastic
import           Control.Lens
import           Control.Error
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.State.Class
import           Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application
import           Util

------------------------------------------------------------------------------
-- | 
handlePoem :: Handler App StochasticText ()
handlePoem = cRender "_poem"

getIntegerParam :: ByteString -> Handler App StochasticText (Maybe Integer)
getIntegerParam paramName = runMaybeT $ do
    paramBS <- MaybeT $ getParam paramName
    let paramText = hush . decodeUtf8' $ paramBS  
        param = paramText >>= liftM fst . hush . decimal
    hoistMaybe param 

handleUpdateRequest :: Handler App StochasticText ()
handleUpdateRequest = do
    miteration <- getIntegerParam "iteration" 
    mcount <- getIntegerParam "count"
    changeBatch <- get 
    let render iteration count = do
        get >>= liftIO . readChangeBatch iteration count >>= writeJSON
            -- >>= return . (^.strict) . encode . toJSON 
    maybe (writeText "FOOO")
          (uncurry render)
          ((,) <$> miteration <*> mcount)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/poem", with poem handlePoem)
         , ("/updates/:iteration/:count", with poem handleUpdateRequest)
         , ("", serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    p <- nestSnaplet "poem" poem initVerses 
    addPoemSplices h poem 
    addRoutes routes
    return $ App h p

