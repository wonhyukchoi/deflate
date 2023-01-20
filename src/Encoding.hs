
--------------------------------------------------------------------------------
module Encoding ( Encoding, serialize, deserialize ) where

--------------------------------------------------------------------------------

import qualified Data.Map as Map

import Unit ( Unit )

--------------------------------------------------------------------------------

data Encoding x xs = Encoding
  { compressionUnit :: Unit x xs
  , huffmanTree :: Map.Map x Int
  , content :: [x]
  }
  deriving (Show)

serialize :: Unit x xs -> xs -> Encoding x xs
serialize = undefined

deserialize :: Encoding x xs -> xs
deserialize = undefined