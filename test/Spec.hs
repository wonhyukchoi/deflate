-----------------------------------------------------------------------------

import Test.Hspec ( hspec )

import qualified Test.PriorityQueue as PriorityQueue
import qualified Test.Huffman as Huffman

-----------------------------------------------------------------------------

main :: IO ()
main =  hspec $ do
    PriorityQueue.test
    Huffman.test