{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-----------------------------------------------------------------------------
module LZ77 ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Base

import qualified Data.Sequence as Seq
import qualified Data.List as List

import Prelude hiding ( drop, take, length )
import Data.Foldable ( toList )
import Data.Sequence ( Seq
                     , cycleTaking
                     , length
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

data Match a = Match
  { offset :: Natural,
    matchLen :: Natural,
    next :: a
  }
  deriving (Show)

type Encoded a = [Match a]

instance Eq (Match a) where
  Match offset1 len1 _ == Match offset2 len2 _ =
    len1 == len2 && offset1 == offset2

instance Ord (Match a) where
  Match offset1 len1 _ `compare` Match offset2 len2 _ =
    let lenComp = len1 `compare` len2
    in case lenComp of
      EQ -> offset2 `compare` offset1
      res -> res

type Dict = Seq
type Processed = Dict

data Progress stream piece = Progress
  { dict:: Dict piece
  , remaining :: stream
  }

instance (Show a, Show b) => Show (Progress a b) where
  show (Progress d r) = unwords ["<DICT:" ++ show d, "REM: " ++ show r ++ ">"]

finished :: Streamable stream => Progress stream piece -> Bool
finished Progress{..} = empty remaining

dictSize :: Int
dictSize = 6

encode
  :: forall stream. (Streamable stream, Eq (Piece stream))
  => stream
  -> [Match (Piece stream)]
encode s = let (_, encoded, _) = encode' (Progress Empty s) [] in reverse encoded
  where 
    encode'
      :: Progress stream (Piece stream)
      -> [Match (Piece stream)]
      -> (Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))
    encode' prog@Progress{..} encoded =
      let result@(progress, e, processed) = maxMatch
      in if finished progress
           then result
           else encode' (changeDict (dict >< processed) progress) e

      where
        changeDict
          :: Dict (Piece stream)
          -> Progress stream (Piece stream)
          -> Progress stream (Piece stream)
        changeDict d (Progress _ r) = Progress d r

        encodeOnce
          :: Natural
          -> (Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))
          -> (Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))
        encodeOnce offset = encodeStep 0
          where
            encodeStep
              :: Natural
              -> (Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))
              -> (Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))
            encodeStep matchLen (Progress currDict remain, currEncoded, processed) = 
              case uncons remain of
              Nothing -> error "Empty input to compression!"
              Just (x, xs) -> let 
                match = Match offset matchLen x
                in case currDict of
                  Empty -> (Progress (singleton x) xs, match:currEncoded, processed)
                  d:<ds-> if d == x && not (empty xs)
                      then encodeStep
                             (matchLen + 1)
                             (Progress ds xs, currEncoded, processed |> x)
                      else (Progress ds xs, match:currEncoded, processed)
        
        maxMatch :: (Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))
        maxMatch = foldr1 max' matches
          where max' :: Ord b => (a, [b], c) -> (a, [b], c) -> (a,[b], c)
                max' (_, [], _) _ = error "This should never happen"
                max' _ (_, [], _) = error "This should never happen"
                max' match1@(_, x:_, _) match2@(_, y:_, _) =
                  if x <= y then match2 else match1

                dicts :: [(Natural, Dict (Piece stream))]
                dicts = (0, Empty):pieces
                  where extension =
                          fromList $ List.take (dictSize - 1) (unpack remaining)
                        
                        nonNullTails = drop 1 $ Seq.reverse $ tails dict

                        pieces = zip [1..] $
                          toList $
                          take dictSize $
                          fmap (>< extension) nonNullTails

                fmapSnd :: (b -> c) -> (a, b) -> (a, c)
                fmapSnd f (a, b) = (a, f b)

                makeInput
                  :: Dict (Piece stream)
                  -> (Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))
                makeInput d = (changeDict d prog, encoded, Empty)

                matches
                  :: [(Progress stream (Piece stream), Encoded (Piece stream), Processed (Piece stream))]
                matches = map (uncurry encodeOnce . fmapSnd makeInput) dicts

decode :: Encoded a -> [a]
decode = toList . decode' Seq.empty Seq.empty
  where
    decode' :: Dict a -> Seq a -> Encoded a -> Seq a
    decode' _ done [] = done
    decode' dict done (Match{..}:ms) = decode' dict' (done >< match) ms
      where
        numCycles = if offset == 0 then 1
          else ceiling $ fromIntegral matchLen / fromIntegral offset
        
        dictBase = drop (length dict - fromIntegral offset) dict

        workingDict = if null dictBase then fromList []
          else cycleTaking numCycles dictBase

        decoded = take (fromIntegral matchLen) workingDict
        match = decoded |> next
        dict' = take dictSize $ dict >< match

newtype Encoding a = Encoding [Match a] deriving (Show)

instance Base.Encoding Encoding where
  compress = Encoding . encode

  decompress (Encoding encoded) = decode encoded