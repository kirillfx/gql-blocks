-- Allows to derive Morpheus DecodeScalar & EncodeScalar that
-- have representation type of Text using deriving via startegy.
module GQLScalarDerivingVia where

import           Data.Coerce
import           Data.Morpheus.Types
import qualified Data.Text           as T
import           Relude


newtype GQLTextScalar a = GQLTextScalar a
  deriving stock Generic


instance Read a => DecodeScalar (GQLTextScalar a) where
  decodeScalar (String x) = case readMaybe . T.unpack $ x of
    Nothing -> Left $ "Can't decode: " <> x
    Just a  -> pure . GQLTextScalar $ a
  decodeScalar _          = Left "invalid Value!"


instance (Show a) => EncodeScalar (GQLTextScalar a) where
  encodeScalar (GQLTextScalar value) = String . show $ value
