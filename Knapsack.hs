module Knapsack where

import Keys
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as B

-- Erweiterter euklidischer Algorithmus
extEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extEuclid a 0 = (a,1,0)
extEuclid a b = let (d,s,t) = extEuclid b (a `mod` b)
                in (d,t,s - (a `div` b) * t)

-- Multiplicative inverse
multInverse :: Integer -> Integer -> Integer
multInverse a b = let (d,s,t) = extEuclid a b
                  in if s < 0
                        then s + b
                        else s

-- Encrypt a list (Bitfield) with the Public Key Sequence
encryptHelp :: [Integer] -> [Integer] -> Integer
encryptHelp k t = sum $ zipWith (*) k t

-- Encrypt a List of 0,1 (Bitfield) with a PublicKey
encrypt :: PublicKey -> [Integer] -> Integer
encrypt k t 
            | (publicKeySize k) /= (length t) = error "Schluessegroesse ungleich Text"
            | otherwise                       = encryptHelp (publicKeySequence k) t

-- Entschluesselt Nachricht mit Super-Absteigender Liste (reverse private sequence)
decryptHelp :: [Integer] -> Integer -> [Integer]
decryptHelp [k] c    | c - k == 0 = [1]
                     | c     == 0 = [0]
                     | otherwise  = error "Fehlerhafte Message - a"
decryptHelp (k:ks) c | c - k >= 0 = (decryptHelp ks (c - k)) ++ [1]
                     | otherwise  = (decryptHelp ks c) ++ [0]

decrypt :: PrivateKey -> Integer -> [Integer]
decrypt k c = decryptHelp (reverse (privateKeySequence k)) (((c * (multInverse (privateKeyR k) (privateKeyQ k))) `mod` (privateKeyQ k)))

encryptString :: PublicKey -> BS.ByteString -> BS.ByteString
encryptString k bs = undefined

decryptString :: PrivateKey -> BS.ByteString -> BS.ByteString
decryptString k bs = undefined

toBitfield :: B.Word8 -> [Integer]
toBitfield = undefined

fromBitfield :: [Integer] -> B.Word8
fromBitfield = undefined
