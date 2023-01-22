-- GHC extensions

-----------------------------------------------------------------------------
module LZ77 ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Base

import Numeric.Natural ( Natural )
import Base ( Unit(..), uncons )

-----------------------------------------------------------------------------

data LZ77 a = LZ77
  { offset :: Natural,
    length :: Natural,
    value :: a
  }
  deriving (Show)

newtype Encoding a = Encoding [LZ77 a] deriving (Show)

instance Base.Encoding Encoding where
  compress :: Streamable s => s -> encoding (Piece s)
  compress = undefined

  decompress :: encoding a -> [a]
  decompress = undefined