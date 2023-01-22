{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
module Huffman ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Base

import Numeric.Natural ( Natural )
import Base ( Streamable(..), uncons )
import PriorityQueue ( PriorityQueue, empty, enqueue, dequeue )

-----------------------------------------------------------------------------

type Frequencies a = Map.Map a Natural

frequencies
  :: (Streamable s, Ord (Piece s))
  => s
  -> Frequencies (Piece s)
frequencies stream = frequencies' stream Map.empty
  where 
    frequencies' stream' freqs = case uncons stream' of
      Nothing -> freqs
      Just (piece, rest) -> frequencies' rest $ Map.insertWith (+) piece 1 freqs

data HuffmanTree a
  = Null
  | Tree a Natural (HuffmanTree a) (HuffmanTree a)
  deriving (Show)

newtype Encoding a = Encoding (HuffmanTree a, [a]) deriving (Show)

instance Base.Encoding Encoding where
  compress = undefined

  decompress = undefined