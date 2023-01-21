-- GHC extensions

-----------------------------------------------------------------------------
module Huffman ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Base

import Numeric.Natural ( Natural )
import Base ( Unit(..), uncons )

-----------------------------------------------------------------------------

type HuffmanTree a = Map.Map a Natural

newtype Encoding a = Encoding (HuffmanTree a, [a]) deriving (Show)

instance Base.Encoding Encoding where
  compress :: Unit x xs -> xs -> Encoding x
  compress = undefined

  decompress :: Encoding x -> xs 
  decompress = undefined