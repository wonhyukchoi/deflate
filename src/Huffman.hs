-- GHC extensions

-----------------------------------------------------------------------------
module Huffman ( HuffmanTree ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map

import Unit ( Unit(..) )

-----------------------------------------------------------------------------

type HuffmanTree a = Map.Map a Int