module Main
( main -- :: IO ()
) where

import Prelude as P
import Test.QuickCheck
import Test.QuickCheck.Property

import Crypto.Elligator

prop_elligatorInvertible = 

main = do
  P.putStrLn "running tests"
  quickCheck prop_elligatorInvertible
