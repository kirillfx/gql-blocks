{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
module GraphQL where

import           Control.Lens
import           Control.Monad.Except
import           Data.Coerce
import           Data.FileEmbed                   (makeRelativeToProject)
import           Data.Morpheus                    (deriveApp, interpreter)
import           Data.Morpheus.Document           (importGQLDocument)
import           Data.Morpheus.Server
import           Data.Morpheus.Types              (App, DecodeScalar,
                                                   EncodeScalar, GQLRequest,
                                                   GQLResponse, QUERY, ResolveQ,
                                                   Resolver, ResolverQ,
                                                   RootResolver (..),
                                                   Undefined (..), Arg(..))
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

mkDivBlock :: Monad m => DivId -> [Block m] -> Block m
mkDivBlock i xs = 
  BlockDivBlock $ DivBlock (pure i) (pure xs)

mkTextBlock :: Monad m => TextId -> Text -> Block m
mkTextBlock i content = 
  BlockTextBlock $ TextBlock (pure i) (pure content)

samplePage :: forall m. Monad m => MonadIO m => Arg "id" (Maybe PageId) -> m (Maybe (Page m))
samplePage _  = pure . Just $ Page
  (pure (PageId "page0"))
  (pure "Page 0")
  (pure $ mkDivBlock (DivId "0") subBlocks)
  where
    subBlocks :: [Block m]
    subBlocks = 
      [ BlockTextBlock $ TextBlock (pure $ TextId "text0") (pure "Content of block 0")
      , BlockDivBlock $ DivBlock (pure $ DivId "div0") (pure
        [ mkTextBlock (TextId "text1") "TextBlock1"
        , mkTextBlock (TextId "text2") "TextBlock2"
        , mkDivBlock (DivId "div2") 
          [ mkTextBlock (TextId "text3") "TextBlock3" 
          ]
        ])
      ]

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query
        { getPage = samplePage
        },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }


graphQLInterpreter ::  GQLRequest -> IO GQLResponse
graphQLInterpreter = interpreter rootResolver


graphQLApp :: App () IO
graphQLApp = deriveApp rootResolver
