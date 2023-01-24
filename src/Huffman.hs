{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
module Huffman ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Base

import Data.List ( unfoldr )
import Data.Maybe ( isNothing )
import Control.Exception ( assert )

import Data.Tuple ( swap )
import Numeric.Natural ( Natural )

import Base ( Streamable(..), uncons )
import PriorityQueue ( PriorityQueue, empty, enqueue, dequeue )

-----------------------------------------------------------------------------

type FreqMap a = Map.Map a Natural

frequencies
  :: (Streamable s, Ord (Piece s))
  => s
  -> FreqMap (Piece s)
frequencies stream = frequencies' stream Map.empty
  where 
    frequencies' stream' freqs = case uncons stream' of
      Nothing -> freqs
      Just (piece, rest) -> frequencies' rest $ Map.insertWith (+) piece 1 freqs

data HuffmanTree a
  = Leaf Natural a
  | Tree Natural (HuffmanTree a) (HuffmanTree a)
  deriving (Show)

weight :: HuffmanTree a -> Natural
weight = \case
  Leaf w _ -> w
  Tree w _ _ -> w

merge :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a
merge left right =
  let weightLeft  = weight left
      weightRight = weight right
      weightSum   = weightLeft + weightRight
  in if weightLeft <= weightRight
       then Tree weightSum left right
       else Tree weightSum right left

instance Eq (HuffmanTree a) where
  a == b = weight a == weight b

instance Ord (HuffmanTree a) where
  a `compare` b = weight a `compare` weight b

frequencyQueue :: FreqMap a  -> PriorityQueue (HuffmanTree a)
frequencyQueue freqs =
  foldr (enqueue . uncurry Leaf . swap) empty $ Map.assocs freqs

fromPQ :: Ord a => PriorityQueue (HuffmanTree a) -> Maybe (HuffmanTree a)
fromPQ pq = case dequeue pq of
  Nothing -> Nothing
  Just (smallest, pq') -> case dequeue pq' of
    Nothing -> Just smallest
    Just (nextSmallest, pq'') ->
      fromPQ $ enqueue (smallest `merge` nextSmallest) pq''

data Direction = TreeLeft | TreeRight deriving ( Show )
type EncodedValue = [Direction]
type EncodingTable a = Map.Map a EncodedValue

indexTree :: Ord a => HuffmanTree a -> EncodingTable a
indexTree = go []
  where go :: Ord a => [Direction] -> HuffmanTree a -> Map.Map a [Direction]
        go before = \case
          Leaf _ a -> Map.singleton a $ reverse before
          Tree _ left right -> go (TreeLeft:before) left <>
                               go (TreeRight:before) right

encode :: Ord a => a -> EncodingTable a -> Maybe EncodedValue
encode = Map.lookup

newtype Encoding a = Encoding (Maybe (EncodingTable a, [EncodedValue]))
  deriving (Show)

instance Base.Encoding Encoding where
  compress stream = Encoding $ case freqs of
    Nothing -> assert (isNothing (uncons stream)) Nothing
    Just tree ->
      let encoder  = indexTree tree
          lookup k = case encode k encoder of
            Nothing -> error "This should never happen"
            Just v  -> v
      in Just (encoder , map lookup $ unfoldr uncons stream)
    where freqs = fromPQ $ frequencyQueue $ frequencies stream

  decompress = undefined