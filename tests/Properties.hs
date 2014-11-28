module Main
( main -- :: IO ()
) where

import Prelude as P
import Test.QuickCheck
import Test.QuickCheck.Property

import Crypto.Elligator
import Crypto.Curve25519


-- TODO 

inputKey :: SecretKey
inputKey = fromBytes $ B.concat $ [B.replicate 20 0, "\1\0", B.replicate 10 0]
    
-- resultKey = PublicKey "\156W\235\178\232X\193\218\v\248\231\150l\213\210IN\228}\ACK\157    By\DC2B\232*\165\207\149\184_"
            
refKey = [156, 87, 235, 178, 232, 88, 193, 218, 11, 248, 231, 150, 108, 213, 210, 73, 7    8, 228, 125, 6, 157, 66, 121, 18, 66, 232, 42, 165, 207, 149, 184, 95]


prop_elligatorInvertible = undefined

main = do
  P.putStrLn "running tests"
  quickCheck prop_elligatorInvertible
