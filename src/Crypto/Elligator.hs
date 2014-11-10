module Crypto.Elligator where

{-
slow implementation using Integer.
-}

import Data.ByteString as BS
import Data.Bits
import Prelude as P

{- all are 32 bytes long. should i properly type this using word 
-}
type PrivateKey = ByteString
type PublicKey = ByteString
type Representative = ByteString

{-
field GF(2 ^ 255 - 19) - it's a galois field
operations:

add
sub 
mul
pow
invert
-}

{-
  nothing for now 
-}


data FE25519 = FE25519 {val :: Integer} deriving (Show, Eq)
m25519 = 2 ^ 255 - 19

applyFE f x = x {val = f $ val x}


instance Num FE25519 where
 negate = applyFE ((\x -> x `mod` m25519) . negate)
 (+) x y = FE25519 $ (val x + val y) `mod` m25519
 (*) x y = FE25519 $ (val x * val y) `mod` m25519
 abs = applyFE abs
 signum = applyFE signum
 fromInteger = FE25519

bits :: ByteString -> [Bool]
bits = P.concat . P.map (\ b -> P.map (testBit b) [0..7]) . BS.unpack

decodeFE25519 :: ByteString -> FE25519
decodeFE25519 = P.sum . P.map (\(p, b) -> if b then 2 ^ p else 0) . P.zip [0..] . bits

generatePubAndRandStr :: PrivateKey -> Maybe (PublicKey, Representative)
generatePubAndRandStr = undefined

representativeToPub :: Representative -> PublicKey
representativeToPub = undefined
