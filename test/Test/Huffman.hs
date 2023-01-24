-----------------------------------------------------------------------------

module Test.Huffman ( test ) where

-----------------------------------------------------------------------------

import Test.QuickCheck ( property )
import Test.Hspec ( SpecWith, describe, it )

import Base ( compress, decompress )
import Huffman ( Encoding )

-----------------------------------------------------------------------------

-- | Property that encode . decode == id.
--   QuickCheck does not natively implement Text, so we use String.
prop_inverse :: String -> Bool
prop_inverse txt = txt == decoded
  where encoded :: Encoding Char
        encoded = compress txt

        decoded :: String
        decoded = decompress encoded

test :: SpecWith ()
test = describe "Huffman encoding test" $ do
  it "sorted check" $ property prop_inverse