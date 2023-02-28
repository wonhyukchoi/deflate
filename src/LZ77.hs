{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-----------------------------------------------------------------------------
module LZ77 ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Base

import qualified Data.Sequence as Seq
import qualified Data.List as List

import Prelude hiding ( drop, take, length )
import Data.Foldable (toList)
import Data.Sequence ( Seq
                     , tails
                     , singleton
                     , fromList
                     , drop
                     , take
                     , (|>)
                     , (><)
                     )
import Numeric.Natural ( Natural )
import Base ( Streamable(..) )

-----------------------------------------------------------------------------

pattern Empty :: Seq a
pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty

pattern (:<) :: a -> Seq a -> Seq a
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|) 

-- | Seq[begin:end), includes start but excludes end.
subseq :: Natural -> Natural -> Seq a -> Seq a
subseq begin end seq' = subseq'
  where begin' = fromIntegral begin
        end' = fromIntegral end
        starts = drop begin' seq'
        subseq' = take (end' - begin' - 1) starts

data Match a = Match
  { offset :: Natural,
    matchLen :: Natural,
    next :: a
  }
  deriving (Show)

type Encoded a = [Match a]

instance Eq (Match a) where
  Match _ len1 _ == Match _ len2 _ = len1 == len2

instance Ord (Match a) where
  Match _ len1 _ `compare` Match _ len2 _ = len1 `compare` len2

type Dict = Seq

data Progress stream piece = Progress
  { dict:: Dict piece
  , remaining :: stream
  }

finished :: Streamable stream => Progress stream piece -> Bool
finished Progress{..} = empty remaining

dictSize :: Int
dictSize = 6

encode
  :: forall stream. (Streamable stream, Eq (Piece stream))
  => stream
  -> [Match (Piece stream)]
encode s = let (_, result) = encode' (Progress Empty s) [] in reverse result
  where 
    encode'
      :: Progress stream (Piece stream)
      -> [Match (Piece stream)]
      -> (Progress stream (Piece stream), Encoded (Piece stream))
    encode' progress@Progress{..} encoded = if empty remaining
      then (progress, encoded)
      else let result@(progress', _) = maxMatch in
        if finished progress'
          then result
          else uncurry encode' result

      where
        dicts :: [(Natural, Dict (Piece stream))]
        dicts = zip [1..] dictPieces
          where dictExtension = List.take (dictSize - 1) (unpack remaining)
                maxDict = dict >< fromList dictExtension
                dictPieces = toList $ tails maxDict
        
        encodeOnce
          :: Natural
          -> (Progress stream (Piece stream), Encoded (Piece stream))
          -> (Progress stream (Piece stream), Encoded (Piece stream))
        encodeOnce offset = encodeStep 0
          where
            encodeStep
              :: Natural
              -> (Progress stream (Piece stream), Encoded (Piece stream))
              -> (Progress stream (Piece stream), Encoded (Piece stream))
            encodeStep currLen (Progress currDict remain, currEncoded) = 
              case uncons remain of
              Nothing -> error "Empty input to compression!"
              Just (x, xs) -> let 
                match = Match offset currLen x
                in case currDict of
                  Empty -> (Progress (singleton x) xs, match:currEncoded)
                  d:<ds -> let newDict = ds |> x
                    in if d == x && not (isSingleton xs)
                      then encodeStep (currLen + 1) (Progress newDict xs, currEncoded)
                      else (Progress newDict xs, match:currEncoded)
        
        maxMatch :: (Progress stream (Piece stream), Encoded (Piece stream))
        maxMatch = foldr1 max' matches
          where max' :: Ord b => (a, [b]) -> (a, [b]) -> (a,[b])
                max' (_, []) _ = error "This should never happen"
                max' _ (_, []) = error "This should never happen"
                max' match1@(_,x:_) match2@(_,y:_) =
                  if x <= y then match2 else match1
                
                changeDict
                  :: Dict (Piece stream)
                  -> (Progress stream (Piece stream), Encoded (Piece stream))
                changeDict dict' = (Progress dict' remaining, encoded)

                fmapSnd :: (b -> c) -> (a, b) -> (a, c)
                fmapSnd f (a, b) = (a, f b)

                matches
                  :: [(Progress stream (Piece stream), Encoded (Piece stream))]
                matches = map (uncurry encodeOnce . fmapSnd changeDict) dicts

decode :: Encoded a -> [a]
decode = toList . decode' Seq.empty Seq.empty
  where
    decode' :: Dict a -> Seq a -> Encoded a -> Seq a
    decode' _ done [] = done
    decode' dict done (Match{..}:ms) = decode' dict' (done >< match) ms
      where end = offset + matchLen + 1
            decoded = subseq offset end dict
            match = decoded |> next
            dict' = take dictSize $ dict >< match

newtype Encoding a = Encoding [Match a] deriving (Show)

instance Base.Encoding Encoding where
  compress = Encoding . encode

  decompress (Encoding encoded) = decode encoded