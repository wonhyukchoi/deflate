{-# LANGUAGE GADTs, StandaloneDeriving, LambdaCase #-}

-----------------------------------------------------------------------------
module Unit ( Unit(..), next ) where

-----------------------------------------------------------------------------

import qualified Data.List as List
import qualified Data.Text as Text

import Data.Text ( Text )

-----------------------------------------------------------------------------

data Unit x xs where
  Character :: Unit Char Text
  Word :: Unit Text [Text]

deriving instance Show (Unit x xs)

next :: Unit x xs -> xs -> Maybe (x, xs)
next = \case
  Character -> Text.uncons
  Word -> List.uncons