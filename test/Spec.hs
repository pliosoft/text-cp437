import Test.QuickCheck
import qualified Data.Text as T
import Data.Text.Arbitrary
import Data.Text.CP437
import qualified Data.Text.CP437.Graphical as G

compareChars :: (Char, Char) -> Bool
compareChars (a, b) = a == b || b == '\x0'

compareGraphicalChars :: (Char, Char) -> Bool
compareGraphicalChars (a, b) = a == b || b == ' '

propReversableInRange :: T.Text -> Bool
propReversableInRange t = all compareChars (T.zip t (cp437ToText (textToCp437 t)))

propReversableInRangeGraphical :: T.Text -> Bool
propReversableInRangeGraphical t = all compareGraphicalChars (T.zip t (G.cp437ToText (G.textToCp437 t)))

main :: IO ()
main = quickCheck propReversableInRange >> quickCheck propReversableInRangeGraphical

