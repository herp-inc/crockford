module Main where

import Codec.Crockford
import Test.QuickCheck
import Text.Printf

tests :: [(String, IO ())]
tests = [
    ("crockfordRoundTrip", quickCheck prop_crockfordRoundTrip)]

main :: IO ()
main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

