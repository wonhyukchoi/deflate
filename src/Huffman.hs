-- GHC extensions

-----------------------------------------------------------------------------
module Huffman ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Base

import Numeric.Natural ( Natural )
import Base ( Streamable(..), uncons )

-----------------------------------------------------------------------------

type Frequencies a = Map.Map a Natural

data HuffmanTree a
  = Leaf a Natural
  | Tree a Natural (HuffmanTree a) (HuffmanTree a)
  deriving (Show)

newtype Encoding a = Encoding (HuffmanTree a, [a]) deriving (Show)

instance Base.Encoding Encoding where
  compress :: Streamable s => s -> encoding (Piece s)
  compress = undefined

  decompress :: encoding a -> [a]
  decompress = undefined