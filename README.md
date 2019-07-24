# text-cp437

Basic mapping of cp437 bytes to UTF and back.

* `Data.Text.CP437` offers mapping that maps control characters to control characters.
* `Data.Text.CP437.Graphical` maps control characters to old school pictures like `♪`.

## `textToCp437 :: Text -> ByteString`

Given a string of text, produce a bytestring containing valid CP-437 encoded data.

If any UTF symbols are used that occur in CP437 they will be translated.

## `cp437ToText :: ByteString -> Text`

Given a ByteString that is encoded with CP-437, will decode to a UTF Text, with all symbols converted.

## `utfToByte :: Char -> Word8`

Convert a single unicode character to a CP-437 byte.

## `byteToUtf :: Word8 -> Char`

Convert a single byte into a unicode character.

