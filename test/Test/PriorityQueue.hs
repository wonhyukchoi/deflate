-----------------------------------------------------------------------------

module Test.PriorityQueue ( test ) where

-----------------------------------------------------------------------------

import qualified Data.List as List

import Test.QuickCheck ( property )
import Test.Hspec ( SpecWith, describe, it )

import PriorityQueue ( PriorityQueue, enqueue, dequeue, empty )

-----------------------------------------------------------------------------

sort :: Ord a => [a] -> [a]
sort xs = let (nondescreasing, _) = sort' [] pq in reverse nondescreasing
  where pq = foldr enqueue empty xs
        sort' :: Ord a => [a] -> PriorityQueue a -> ([a], PriorityQueue a)
        sort' ys pq' = case dequeue pq' of
          Nothing -> (ys, pq')
          Just (y, pq'') -> sort' (y:ys) pq''

-- | Property that enqueueing into a PQ and dequeueing results in sorting
--   A more general type signature is Ord a => [a] -> Bool,
--   but quickcheck relies on monomorphic typing.
--   It is possible to use `Test.QuickCheck.All.monomorphic`, 
--   but that requires TH and is bound to the GHC staging restriction,
--   therefore it is omitted here.
prop_sortedInt :: [Integer] -> Bool
prop_sortedInt xs = List.sort xs == sort xs

test :: SpecWith ()
test = describe "Priority Queue sort check" $ do
  it "sorted check" $ property prop_sortedInt