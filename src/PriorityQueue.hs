{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
module PriorityQueue ( PriorityQueue, empty, enqueue, dequeue ) where

-----------------------------------------------------------------------------

-- | A Priority Queue, currently implemented as a Skew Heap.
type PriorityQueue a = SkewHeap a

empty :: PriorityQueue a
empty = Empty

enqueue :: Ord a => a -> PriorityQueue a -> PriorityQueue a
enqueue = insert

dequeue :: Ord a => PriorityQueue a -> Maybe (a, PriorityQueue a)
dequeue = popMin

-- | A Min Skew Heap.
data SkewHeap a
  = Empty
  | Node a (SkewHeap a) (SkewHeap a)

singleton :: a -> SkewHeap a
singleton a = Node a Empty Empty

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge left right = case (left, right) of
  (Empty, right') -> right'
  (left', Empty) -> left'
  (Node lRoot ll lr, Node rRoot rl rr) -> if lRoot < rRoot
    then Node lRoot (ll `merge` right) lr
    else Node rRoot (rl `merge` left) rr

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x heap = singleton x `merge` heap

popMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
popMin = \case
  Empty -> Nothing
  Node root left right -> Just (root, left `merge` right)