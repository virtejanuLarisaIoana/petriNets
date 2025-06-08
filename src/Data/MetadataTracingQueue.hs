{- | Module: Data.MetadataTracingQuee
Description: A queue that appends tracibility data to tokens as they move
    through a petri net.

When giving examples, we add to the "right" of the queue and remove from the "left", i.e.:

    start (first out)                             end (last in)
    [("foo", 3), ("bar", 5), ................... ("qux", 10)]
-}
module Data.MetadataTracingQueue (
    empty,
    lengthQ,
    null,
    singleton,
    MTQ,
    pushMTQ,
    take,
    drop,
    split,
    fromList,
    toListMTQ, 
    reRoot,
    push
) where

import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Internal as Seqi
import Prelude hiding (drop, null, take)
import Data.Tree

{- | A FIFO queue holding a single semantic "type" of object
(i.e., a petri net token of a particular type) where multiple objects can share metadata.
-}
newtype MTQ metadata = MTQ {metadata :: Seq.Seq (Tree metadata, Integer)}
  deriving (Eq, Show)


fromList :: [(Tree metadata, Integer)] -> MTQ metadata
fromList l = MTQ $ Seq.fromList l

toListMTQ :: MTQ metadata -> [(Tree metadata, Integer)]
toListMTQ (MTQ m) 
  | Seq.null m = []
  | otherwise    = Seq.index m 0 : toListMTQ (MTQ (Seq.drop 1 m))

empty :: MTQ m
empty = MTQ Seq.Empty

singleton :: Tree m -> Integer -> MTQ m 
singleton meta quantity = MTQ $ Seq.singleton (meta, quantity)

{- | Add an element to the front of the queue

Example ::
   push "qux" 10 [("foo", 3), ("bar, 10")] == [("foo", 3), ("bar", 10), ("qux", 10)]
-}
push :: Tree m -> Integer -> MTQ m -> MTQ m
push meta quantity (MTQ existingQueue) = MTQ $ existingQueue Seq.|> (meta, quantity)

reRoot :: m -> [Tree m] -> Tree m
reRoot = Node 

pushMTQ :: 
  -- | The existing queue
  MTQ m 
  -- | The queue to append as the new tail
  -> MTQ m
  -- | The resulting queue 
  -> MTQ m
pushMTQ (MTQ existingQueue) (MTQ newTail)  = MTQ $ existingQueue Seq.>< newTail 

-- pop 3 [("foo", 1), ("bar", 5), ("qux", 10)] == [("foo", 1), ("bar", 2)]
take :: Integer -> MTQ m -> MTQ m
take q (MTQ mtq)
    | q <= 0 = empty
    | otherwise = MTQ $ go q mtq
  where
    go :: Integer -> Seq (m, Integer) -> Seq (m, Integer)
    go _ Seq.Empty = Seq.Empty
    go q' ((metadata, quantity) Seq.:<| tail) = case compare q' quantity of
        GT -> (metadata, quantity) <| go (q' - quantity) tail
        _ -> (metadata, q') <| Seq.Empty

-- -- pop 3 [("foo", 1), ("bar", 5), ("qux", 10)] == [("bar", 3), ("qux",10)]
drop :: Integer -> MTQ m -> MTQ m
drop q m@(MTQ mtq)
    | q <= 0 = m 
    | otherwise = MTQ $ go q mtq
  where
    go q' Seq.Empty = Seq.Empty
    go q' ((metadata, quantity) Seq.:<| tail)
        | q' >= quantity = go (q' - quantity) tail
        | otherwise = (metadata, quantity - q') <| tail

-- split 3 [("foo", 1), ("bar", 5), ("qux", 10)]
--     == ([("foo", 1), ("bar", 2)], [("bar", 3), ("qux",10)])
split :: Integer -> MTQ m -> (MTQ m, MTQ m)
split q (MTQ mtq) =
    ( Data.MetadataTracingQueue.take q (MTQ mtq)
    , Data.MetadataTracingQueue.drop q (MTQ mtq)
    )

null :: MTQ m -> Bool
null (MTQ mtq) = Seq.null mtq

lengthQ :: MTQ m -> Integer
lengthQ (MTQ Seq.Empty) = 0
lengthQ (MTQ ((metadata, quantity) Seq.:<| tail)) = quantity + lengthQ (MTQ tail)
