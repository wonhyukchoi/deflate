-----------------------------------------------------------------------------

import Test.Hspec ( hspec )

import qualified Test.PriorityQueue as PriorityQueue
import qualified Test.Huffman as Huffman
import qualified Test.LZ77 as LZ77

-----------------------------------------------------------------------------

main :: IO ()
main =  hspec $ do
    PriorityQueue.test
    Huffman.test
    LZ77.test