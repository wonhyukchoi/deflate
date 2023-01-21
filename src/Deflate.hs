-- GHC extensions

-----------------------------------------------------------------------------
module Deflate ( Encoding(..) ) where

-----------------------------------------------------------------------------

import qualified Base
import qualified Huffman
import qualified LZ77

import Base ( Unit(..) )

-----------------------------------------------------------------------------

newtype Encoding a = Encoding (Huffman.Encoding a, LZ77.Encoding a) -- placeholder

instance Base.Encoding Encoding where
  compress :: Unit x xs -> xs -> Encoding x
  compress = undefined

  decompress :: Encoding x -> xs 
  decompress = undefined