module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           GraphQL
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Relude
import           Servant
import           Server

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api (graphQLServer graphQLApp)

api :: Proxy API
api = Proxy
