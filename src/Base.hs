{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
module Base ( Streamable(..), Encoding(..) ) where

-----------------------------------------------------------------------------

import qualified Data.List as List
import qualified Data.Text as Text

import Data.Text ( Text )
import Data.Kind ( Type )

-----------------------------------------------------------------------------

class Streamable s where
  type Piece s :: Type
  uncons :: s -> Maybe (Piece s, s)

instance Streamable Text where
  type Piece Text = Char
  uncons = Text.uncons

instance Streamable [a] where
  type Piece [a] = a
  uncons = List.uncons

class Encoding encoding where
  compress :: (Streamable s, Ord (Piece s)) => s -> encoding (Piece s)
  decompress :: (Show a) => encoding a -> [a]