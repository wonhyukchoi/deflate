{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
module LZ77 ( Encoding ) where

-----------------------------------------------------------------------------

import qualified Base

import Data.List ( tails )
import Control.Monad ( when )
import Control.Monad.Trans.State (State
                                 , runState
                                 , execState
                                 , get
                                 , gets
                                 , put
                                 , modify
                                 )
import Numeric.Natural ( Natural )
import Base ( Streamable(..) )

-----------------------------------------------------------------------------

data Match a = Match
  { offset :: Natural,
    matchLen :: Natural,
    next :: a
  }
  deriving (Show)

instance Eq (Match a) where
  Match _ len1 _ == Match _ len2 _ = len1 == len2

instance Ord (Match a) where
  Match _ len1 _ `compare` Match _ len2 _ = len1 `compare` len2

type Dict a = [a]

data LZ77 stream piece = LZ77
  { encoded :: [Match piece]
  , dict :: Dict piece
  , remaining :: stream
  }

updateDict :: Dict piece -> LZ77 stream piece -> LZ77 stream piece
updateDict newDict LZ77{..} = LZ77 encoded newDict remaining

type LZ77State stream piece = State (LZ77 stream piece) (Match piece)

dictSize :: Int
dictSize = 6

encode :: forall a. (Eq (Piece a), Streamable a) => LZ77State a (Piece a)
encode = do
  st@LZ77{..} <- get
  if empty remaining
    then return $ head encoded
    else do
      let dicts :: [(Natural, Dict (Piece a))]
          dicts = zipWith
            (\offset dict -> (offset, dict ++ unpacked))
            [1..]
            (tails dict)
            where unpacked = unpack remaining

          maxMatch :: LZ77State a (Piece a)
          maxMatch = maximum <$> traverse runMatch dicts

      put $ execState maxMatch st
      encode

  where 
    runMatch :: (Natural, Dict (Piece a)) -> LZ77State a (Piece a)
    runMatch (offset, newDict) = modify (updateDict newDict) >> findMatch offset

    findMatch :: Natural -> LZ77State a (Piece a)
    findMatch offset = findMatch' 0
      where
        -- FIXME: horribly inefficient.
        findMatch' :: Natural -> LZ77State a (Piece a)
        findMatch' currLen = do
          LZ77{..} <- get
          let dict' = take dictSize $ dict ++ unpack remaining
          case uncons remaining of
            Nothing -> error "Empty input to compression!"
            Just (x, xs) -> do
              let match = Match offset currLen x
                  encoded' = match:encoded
                  finish = put (LZ77 encoded' dict' xs) >> return match
              case dict of
                [] -> finish
                (d:ds) -> if d == x && not (isSingleton xs)
                  then do
                    put $ LZ77 encoded (ds ++ [x]) xs
                    findMatch' (currLen + 1)
                  else finish

newtype Encoding a = Encoding [Match a] deriving (Show)

instance Base.Encoding Encoding where
  compress = undefined

  decompress = undefined