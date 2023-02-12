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
                                 , put
                                 , modify
                                 )
import Numeric.Natural ( Natural )
import Base ( uncons )

-----------------------------------------------------------------------------

data Match a = Match
  { offset :: Natural,
    matchLen :: Natural,
    next :: a
  }
  deriving (Show)

type Dict a = [a]

data LZ77 a = LZ77
  { encoded :: [Match a]
  , dict :: Dict a
  , remaining :: [a]
  }

type LZ77State a = State (LZ77 a) (Match a)

dictSize :: Int
dictSize = 6

encode :: forall a. Eq a => LZ77State a
encode = do
  st@LZ77{..} <- get
  case remaining of
    [] -> return $ head encoded
    _  -> do
      let dicts :: [(Natural, Dict a)]
          dicts = zipWith
            (\offset dict -> (offset, dict ++ remaining))
            [1..]
            (tails dict)
          
          maxMatch :: LZ77State a
          maxMatch = foldr1 compareSt $ map runMatch dicts
  
      put $ execState maxMatch st
      encode

  where 
    compareSt :: LZ77State a -> LZ77State a -> LZ77State a
    compareSt st1 st2 = do
      match1 <- st1
      match2 <- st2
      if matchLen match1 <= matchLen match2
        then st2
        else st1

    runMatch :: (Natural, Dict a) -> LZ77State a
    runMatch (offset, newDict) = modify (updateDict newDict) >> findMatch offset

    updateDict :: Dict a -> LZ77 a -> LZ77 a
    updateDict newDict LZ77{..} = LZ77 encoded newDict remaining

    findMatch :: Natural -> LZ77State a
    findMatch offset = findMatch' 0
      where
        findMatch' :: Natural -> LZ77State a
        findMatch' currLen = do
          LZ77{..} <- get
          let dict' = take dictSize $ dict ++ remaining -- FIXME
          case remaining of
            [] -> error "Empty input to compression!"
            (x:xs) -> do
              let match = Match offset currLen x
                  encoded' = match:encoded
                  finish = put (LZ77 encoded' dict' xs) >> return match
              case dict of
                [] -> finish
                (d:ds) -> if d == x && xs /= []
                  then do
                    put $ LZ77 encoded ds xs
                    findMatch' (currLen + 1)
                  else finish

newtype Encoding a = Encoding [Match a] deriving (Show)

instance Base.Encoding Encoding where
  compress = undefined

  decompress = undefined