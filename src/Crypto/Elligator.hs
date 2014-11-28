{-# LANGUAGE OverloadedStrings #-}

module Crypto.Elligator
  ( elligator
  , elligatorInv
  ) where

{-
slow implementation using Integer.
-}

import Crypto.Curve25519
import Crypto.Number.Serialize

import Data.ByteString as BS
import Data.Bits
import Prelude as P
import Math.NumberTheory.Moduli (sqrtModP)
import Data.Maybe
import Data.LargeWord

type Representative = ByteString

{-
field GF(2 ^ 255 - 19) - it's a galois field

The inverse map
limitations - only points with these properties have uniform string representation:
  1. u /= -A. (The value A is a parameter of Curve25519 and has the value 486662.)
  2. -2u(u + A) is a square
-}

elligatorInv :: SecretKey -> Maybe (PublicKey, Representative)
elligatorInv sk = if (isSquare $ 2 * u * (u + a))
                  then Just (fieldPToPublicKey u, repr)
                  else Nothing
  where
    pubPt :: Point FieldPSq
    pubPt@(Pt squ sqv) = (secretKeyToInteger sk) .* basePt -- can it be InfPt? 
    u = castDown squ -- TODO: is this always safe?
    v = castDown sqv
    repr =  intToBS $ fromJust $
            (flip sqrtModP) characteristic $ fieldToInt $ negate $
            if v < (fromInteger $ (characteristic - 1) `div` 2)
            then u * (recip $ 2 * (u + a))
            else (u + a) * (recip $ 2 * u)

-- the forward map
elligator :: Representative -> PublicKey
elligator repr = fieldPToPublicKey $ epsi * d  - (1 - epsi) * a * (recip 2)
  where
    r = fromInteger $ bsToInt repr
    d :: FieldP
    d = negate $ a * (recip $ 1 + 2 * r ^ 2)
    epsi = squareExp (d ^ 3 + a *  d * 2 + d) 
    

{- the result of squareExp is either 1 or -1 
  if it's 1 then x is a square (modulo characteristic)
-}

squareExp x = x ^ ((characteristic - 1) `div` 2)
isSquare = (== 1) . squareExp

