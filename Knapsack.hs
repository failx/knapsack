module Knapsack where

import Data.Tuple
import Keys

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

-- Encrypt and a list (Bitfield) with the Public Key Sequence
encryptHelp :: [Integer] -> [Integer] -> Integer
encryptHelp [] []         = 0
encryptHelp [] _          = error "Schluessel zu kurz"
encryptHelp _ []          = error "Bitfield zu kurz"
encryptHelp [k] [t]       = (*) k t
encryptHelp (k:ks) (t:ts) = (+) ( (*) k t)  (encryptHelp ks ts)

-- Encrypt a List of 0,1 (Bitfield) with a PublicKey
encrypt :: PublicKey -> [Integer] -> Integer
encrypt k t 
            | (publicKeySize k) /= (length t) = error "Schluessegroesse ungleich Text"
            | otherwise                          = encryptHelp (publicKeySequence k) t

-- Entschluesselt Nachricht mit Super-Absteigender Liste (reverse private sequence)
decryptHelp :: [Integer] -> Integer -> [Integer]
decryptHelp [k] c    | c - k == 0 = [1]
                     | c     == 0 = [0]
                     | otherwise  = error "Fehlerhafte Message - a"
decryptHelp (k:ks) c | c - k >= 0 = (decryptHelp ks (c - k)) ++ [1]
                     | otherwise  = (decryptHelp ks c) ++ [0]

decrypt :: PrivateKey -> Integer -> [Integer]
decrypt k c = decryptHelp (reverse (privateKeySequence k)) (((c * (multInverse (privateKeyR k) (privateKeyQ k))) `mod` (privateKeyQ k)))

encryptString :: PrivateKey -> String -> String
encryptString = undefined

decryptString :: PublicKey -> String -> String
decryptString = undefined
