{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
module Base ( Streamable(..), Encoding(..) ) where

-----------------------------------------------------------------------------

import qualified Data.List as List
import qualified Data.Text as Text

import Data.Text ( Text )
import Data.Kind ( Type )
import Data.Maybe ( isNothing )

-----------------------------------------------------------------------------

class Streamable s where
  type Piece s :: Type
  uncons :: s -> Maybe (Piece s, s)

  empty :: s -> Bool
  empty = isNothing . uncons

  isSingleton :: s -> Bool
  isSingleton s = case uncons s of
    Nothing -> False
    Just (_, s') -> empty s'

  unpack :: s -> [Piece s]
  unpack stream = reverse $ unpack' [] stream
    where unpack' :: [Piece s] -> s -> [Piece s]
          unpack' prev stream' = case uncons stream' of
            Nothing -> prev
            Just (x, xs) -> unpack' (x:prev) xs

instance Streamable Text where
  type Piece Text = Char
  uncons = Text.uncons

instance Streamable [a] where
  type Piece [a] = a
  uncons = List.uncons

class Encoding encoding where
  compress :: (Streamable s, Ord (Piece s)) => s -> encoding (Piece s)
  decompress :: (Show a) => encoding a -> [a]