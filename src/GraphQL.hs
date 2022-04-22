{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
module GraphQL where

import           Control.Lens
import           Control.Monad.Except
import           Data.Coerce
import           Data.FileEmbed                   (makeRelativeToProject)
import           Data.Morpheus                    (deriveApp, interpreter)
import           Data.Morpheus.App
import           Data.Morpheus.Document           (importGQLDocument)
import           Data.Morpheus.Server
import           Data.Morpheus.Subscriptions      (Event (..), Hashable)
import           Data.Morpheus.Types              (App, DecodeScalar,
                                                   EncodeScalar, GQLRequest,
                                                   GQLResponse, QUERY, ResolveQ,
                                                   Resolver, ResolverQ,
                                                   RootResolver (..),
                                                   Undefined (..))
import           Data.Morpheus.Types.Internal.AST (GQLError (..), Msg (msg))
import           Data.String
import           GQLScalarDerivingVia
import           Relude                           hiding (Undefined)

newtype PageId = PageId Text
  deriving stock (Generic, Show, Read)
  deriving (DecodeScalar, EncodeScalar)
    via GQLTextScalar PageId

newtype VideoId = VideoId Text
  deriving stock (Generic, Show, Read)
  deriving (DecodeScalar, EncodeScalar)
    via GQLTextScalar VideoId

newtype DivId = DivId Text
  deriving stock (Generic, Show, Read)
  deriving (DecodeScalar, EncodeScalar)
    via GQLTextScalar DivId

newtype ImageId = ImageId Text
  deriving stock (Generic, Show, Read)
  deriving (DecodeScalar, EncodeScalar)
    via GQLTextScalar ImageId

newtype TextId = TextId Text
  deriving stock (Generic, Show, Read)
  deriving (DecodeScalar, EncodeScalar)
    via GQLTextScalar TextId


makeRelativeToProject "schema/schema.gql" >>= importGQLDocument

-- samplePage :: GetPageArgs -> IO (Maybe (Page IO))
samplePage :: MonadIO m => GetPageByIdArgs -> m (Maybe (Page m))
-- samplePage = error "Not implemented"
samplePage _  = pure . Just $ Page
  (pure (PageId "page0"))
  (pure "Page 0")
  -- (error "Not implemented")
  (pure
    [ BlockTextBlock $ TextBlock (pure $ TextId "text0") (pure "Content of block 0")
    , BlockDivBlock $ DivBlock (pure $ DivId "div0") (pure
      [ BlockTextBlock $ TextBlock (pure $ TextId "text2") (pure "Content of block 1")
      , BlockTextBlock $ TextBlock (pure $ TextId "text2") (pure "Content of block 2")
      ])
    ]
  )


rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query
        { getPageById = samplePage
        , getPage = samplePage undefined
        },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }


graphQLInterpreter ::  GQLRequest -> IO GQLResponse
graphQLInterpreter = interpreter rootResolver


graphQLApp :: App () IO
graphQLApp = deriveApp rootResolver
