-- | This module implements Douglas Crockford's base32 encoding scheme to
--   store integers as text. See <http://www.crockford.com/wrmg/base32.html>
--   for more details on how this scheme works.
module Codec.Crockford (encode, decode, prop_crockfordRoundTrip) where

import Data.Char
import Data.Digits (digits, unDigits)
import Data.Maybe
import Test.QuickCheck

-- | Decodes a Crockford-encoded String into an integer, if possible. Returns
--   Nothing if the string is not a valid Crockford-encoded value.
decode :: Integral i => String -> Maybe i
decode s = mapM decodeChar s >>= return . unDigits 32

-- | Encodes an integer into a String, using Douglas Crockford's base32
--   encoding.
encode :: Integral i => i -> String
encode = fromJust . mapM encodeChar . digits 32

decodeChar :: Integral i => Char -> Maybe i
decodeChar c = case toUpper c of
    '0' -> Just 0
    'O' -> Just 0
    '1' -> Just 1
    'I' -> Just 1
    'L' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    'A' -> Just 10
    'B' -> Just 11
    'C' -> Just 12
    'D' -> Just 13
    'E' -> Just 14
    'F' -> Just 15
    'G' -> Just 16
    'H' -> Just 17
    'J' -> Just 18
    'K' -> Just 19
    'M' -> Just 20
    'N' -> Just 21
    'P' -> Just 22
    'Q' -> Just 23
    'R' -> Just 24
    'S' -> Just 25
    'T' -> Just 26
    'V' -> Just 27
    'W' -> Just 28
    'X' -> Just 29
    'Y' -> Just 30
    'Z' -> Just 31
    _ -> Nothing

encodeChar :: Integral i => i -> Maybe Char
encodeChar i = case i of
    0  -> Just '0'
    1  -> Just '1'
    2  -> Just '2'
    3  -> Just '3'
    4  -> Just '4'
    5  -> Just '5'
    6  -> Just  '6'
    7  -> Just '7'
    8  -> Just '8'
    9  -> Just '9'
    10 -> Just 'A'
    11 -> Just 'B'
    12 -> Just 'C'
    13 -> Just 'D'
    14 -> Just 'E'
    15 -> Just 'F'
    16 -> Just 'G'
    17 -> Just 'H'
    18 -> Just 'J'
    19 -> Just 'K'
    20 -> Just 'M'
    21 -> Just 'N'
    22 -> Just 'P'
    23 -> Just 'Q'
    24 -> Just 'R'
    25 -> Just 'S'
    26 -> Just 'T'
    27 -> Just 'V'
    28 -> Just 'W'
    29 -> Just 'X'
    30 -> Just 'Y'
    31 -> Just 'Z'
    _ -> Nothing

-- | A QuickCheck property that asserts a positive integer encoded and then
--   decoded using this module remains the same number.
prop_crockfordRoundTrip
    :: Integer -- ^ The integer to test.
    -> Property
prop_crockfordRoundTrip i = i > 0 ==> Just i == (decode . encode) i

