-- | Utils for building aeson parsers

module Aeson
where

import Data.Maybe
import Data.Text
import qualified Data.Traversable as Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson
import Data.Aeson.Types
import Control.Monad

-- | Make parser for vector from parser for its elements
vector :: (Value -> Parser a) -> Value -> Parser (Vector a)
vector p = withArray "array" $ Vector.mapM p

-- | Make parser for list from parser for its elements
list :: (Value -> Parser a) -> Value -> Parser [a]
list p = fmap Vector.toList . vector p

-- | Make parser for object key from key name and parser for its value
--
-- @key "key1" (list int)@
key :: Text -> (Value -> Parser a) -> Value -> Parser a
key name p = withObject "object" $ \o ->
  case HashMap.lookup name o of
    Nothing -> fail "not found"
    Just v -> p v

-- | Make parser for optional object key from key name and parser for its value
opt :: Text -> (Value -> Parser a) -> Value -> Parser (Maybe a)
opt name p = withObject "object" $ \o ->
  case HashMap.lookup name o of
    Nothing -> return Nothing
    Just v -> Just <$> p v

-- | Make parse from optional parser and the default value
--
-- @opt "k" (vector double) v \`def\` Vector.empty@
def :: Parser (Maybe a) -> a -> Parser a
def p d = fromMaybe d <$> p

-- | Make parse from optional parser and the default value
--
-- @vector (opt "k" int \`or\` 0)@
or :: (Value -> Parser (Maybe a)) -> a -> Value -> Parser a
or p d v = fromMaybe d <$> p v

-- | Parser for 'Int'
int :: Value -> Parser Int
int = parseJSON

-- | Parser for 'Double'
double :: Value -> Parser Double
double = parseJSON

-- | Parser for 'Text'
text :: Value -> Parser Text
text = parseJSON

-- | Make parser for lookup list from parser for its value
lookupList :: (Value -> Parser a) -> Value -> Parser [(Text, a)]
lookupList p = withObject "object" $ \o ->
  forM (HashMap.toList o) $ \(k, v) -> do
    res <- p v
    return (k, res)

-- | Make parser for 'HashMap' from parser for its value
hashMap :: (Value -> Parser a) -> Value -> Parser (HashMap Text a)
hashMap p = withObject "object" $ \o ->
  Traversable.mapM p o
