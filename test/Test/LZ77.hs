-----------------------------------------------------------------------------

module Test.LZ77 ( test ) where

-----------------------------------------------------------------------------

import Test.QuickCheck ( Property, property, (==>) )
import Test.Hspec ( SpecWith, describe, it )

import Base ( compress, decompress )
import LZ77 ( Encoding )

import Debug.Trace ( trace )

-----------------------------------------------------------------------------

-- | Property that encode . decode == id.
--   QuickCheck does not natively implement Text, so we use String.
prop_inverse :: String -> Property
prop_inverse txt = not (null txt) ==> txt == decompress encoded
  where encoded :: Encoding Char
        encoded = compress txt

test :: SpecWith ()
test = describe "LZ77 encode/decode inverse test" $ do
  it "checking..." $ property prop_inverse