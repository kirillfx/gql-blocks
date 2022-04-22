{-# LANGUAGE FlexibleContexts #-}
module Server where

import           Data.ByteString.Lazy.Char8  (ByteString)
import           Data.Morpheus.Server        (httpPlayground)
-- import           Data.Morpheus.Subscriptions (PubApp, SubApp, httpPubApp)
import           Data.Morpheus.Types         (App, GQLRequest, GQLResponse,
                                              render)
import qualified Data.Text.Lazy              as LT
import           GHC.TypeLits                (Symbol)
import           GraphQL                     (graphQLApp, graphQLInterpreter)
import           Network.HTTP.Media          ((//), (/:))
import           Network.WebSockets          (ServerApp,
                                              defaultConnectionOptions)
import           Relude                      hiding (ByteString)
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic


data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id


type GraphQLAPI = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

type Schema = "schema" :> Get '[PlainText] Text

type Playground = Get '[HTML] ByteString

-- type GraphQLAPI (name :: Symbol) = name :> (GraphQLAPI' :<|> Schema :<|> Playground)

data GraphQLRoutes route = GraphQLRoutes
  { _graphql     :: route :- GraphQLAPI
  , _schema      :: route :- Schema
  , _playgroaund :: route :- Playground
  }
  deriving stock (Generic)

type API = ToServantApi GraphQLRoutes

graphQLServer
  :: App e IO
  -> ToServant GraphQLRoutes AsServer
graphQLServer app =
  genericServerT $
    GraphQLRoutes
      { _graphql = liftIO . graphQLHandler -- liftIO . httpPubApp publish app
      , _schema = withSchema app
      , _playgroaund = pure httpPlayground
      }

graphQLHandler :: GQLRequest -> IO GQLResponse
graphQLHandler = graphQLInterpreter

-- serveGraphQL ::
--   ( SubApp ServerApp e,
--     PubApp e
--   ) =>
--   [e -> IO ()] ->
--   App e IO ->
--   Server (GraphQLAPI name)
-- serveGraphQL publish app = (liftIO . httpPubApp publish app) :<|> withSchema app :<|> pure httpPlayground

withSchema :: (Applicative f) => App e m -> f Text
withSchema = pure . LT.toStrict . decodeUtf8 . render
