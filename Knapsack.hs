module Knapsack (test) where

import System.Random
import System.IO
import System.IO.Unsafe
import Data.Tuple

-- Test, Schneier
privateKey = PrivateKey 6 105 31 [2, 3, 6, 13, 27, 52]
publicKey  = genPublicKey privateKey
test = [[0,0,0,0,0,0] == (decrypt privateKey $ encrypt publicKey [0,0,0,0,0,0]),
        [0,0,0,0,0,1] == (decrypt privateKey $ encrypt publicKey [0,0,0,0,0,1]),
        [0,0,0,0,1,0] == (decrypt privateKey $ encrypt publicKey [0,0,0,0,1,0]),
        [0,0,0,1,0,0] == (decrypt privateKey $ encrypt publicKey [0,0,0,1,0,0]),
        [0,0,1,0,0,0] == (decrypt privateKey $ encrypt publicKey [0,0,1,0,0,0]),
        [0,1,0,0,0,0] == (decrypt privateKey $ encrypt publicKey [0,1,0,0,0,0]),
        [1,0,0,0,0,0] == (decrypt privateKey $ encrypt publicKey [1,0,0,0,0,0]),
        [0,1,1,0,0,0] == (decrypt privateKey $ encrypt publicKey [0,1,1,0,0,0]),
        [1,1,0,1,0,1] == (decrypt privateKey $ encrypt publicKey [1,1,0,1,0,1]),
        [1,0,1,1,1,0] == (decrypt privateKey $ encrypt publicKey [1,0,1,1,1,0])]

-- Datenstruktur fuer eine privaten Schluessel
data PrivateKey = PrivateKey Int Integer Integer [Integer] deriving (Show) 

-- Datenstruktur fuer eine oeffentlichen Schluessel
data PublicKey  = PublicKey Int [Integer] deriving (Show) 

--Getter fuer Key Size
getPrivateKeySize :: PrivateKey -> Int
getPrivateKeySize (PrivateKey s _ _ _) = s

-- Getter fuer modulo Q
getQ :: PrivateKey -> Integer  
getQ (PrivateKey _ q _ _) = q

-- Getter fuer multiplikator R 
getR :: PrivateKey -> Integer  
getR (PrivateKey _ _ r _) = r

-- Getter fuer Secret Sequence
getSecretSequence :: PrivateKey -> [Integer]  
getSecretSequence (PrivateKey _ _ _ sq) = sq

-- Save Private Key in "private.key"
-- 1. Zeile: Parameter Q
-- 2. Zeile: Parameter R
-- 2. Zeile: Superincreasing Knapsack Sequence
savePrivateKey :: PrivateKey -> IO()
savePrivateKey k = do
                   handle <- openFile "private.key" WriteMode
                   hPutStrLn handle $ show $ getQ k
                   hPutStrLn handle $ show $ getR k
                   hPutStrLn handle $ show $ getSecretSequence k
                   hClose handle
                   
-- Getter fuer Public Sequence
getPublicSequence :: PublicKey -> [Integer]
getPublicSequence (PublicKey _ sq) = sq

--Getter fuer Key Size
getPublicKeySize :: PublicKey -> Int
getPublicKeySize (PublicKey s _) = s

-- Save Public Key
-- 1. Zeile: Hard Knapsack Sequence
savePublicKey :: PublicKey -> IO()
savePublicKey k = do
                  handle <- openFile "public.key" WriteMode
                  hPutStrLn handle $ show $ getPublicSequence k
                  hClose handle

-- Euklidischer Algorithmus
euclid :: Integer -> Integer -> Integer
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

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

-- Random Number Generator, basierend auf System.Random
-- Es ist keine Kryptographischen sicheren Zufahllszahlengenerator
myRND :: Integer -> Integer -> IO Integer
myRND a b = do 
            rnd <- randomRIO (a, b) :: IO Integer
            return rnd

-- Random Number Generator mit min/max, IO Integer wird as unsafePerformeIO umgewandelt
rndMinMax :: Integer -> Integer -> Integer
rndMinMax a b = unsafePerformIO $ myRND a b

-- Random Number Generator mit min und 1024-fach von min als max
-- bei 0 als minimum wird 2^64 als max verwendet
rndMin :: Integer -> Integer
rndMin 0 = rndMinMax 0 $ 2 ^ 64
rndMin a = rndMinMax a $ (*) a 1024

-- Genereate a Super Increasing Sequence, Summe der vorgerigen Werte wird mit uebergeben
superincHelp :: Int -> Integer -> [Integer]
superincHelp 0 _ = []
superincHelp a b = let x = rndMin b
                   in  x : superincHelp (a - 1) (b + x)

-- Generiert Super Increasing Sequence mit Anzahl der Elemente, die generiert werden
superinc :: Int -> [Integer]
superinc a = superincHelp a 0

-- Generiere aus der Super Increasing Sequence den Parameter Q
generateQ :: [Integer] -> Integer
generateQ a = rndMin $ sum a
            

-- Generiere aus dem Parameter Q den Parameter R
generateR :: Integer -> Integer
generateR 0 = 0
generateR a = let x = rndMinMax 1 (a-1)
              in if euclid x a == 1
                        then x
                        else generateR a

-- Generiere einen Privaten Schluessel
genPrivateKey :: Int -> PrivateKey
genPrivateKey a = let newSeq = superinc a
                  in let newQ = generateQ newSeq
                     in let newR = generateR newQ
                        in PrivateKey a newQ newR newSeq

-- Generiere aus dem Privaten Schluessel den Oeffentlichen Schluessel
genPublicKey :: PrivateKey -> PublicKey
genPublicKey a = PublicKey (getPrivateKeySize a)  (map (`mod` getQ a) $ map (getR a *) (getSecretSequence a))

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
            | (getPublicKeySize k) /= (length t) = error "Schluessegroesse ungleich Text"
            | otherwise                          = encryptHelp (getPublicSequence k) t

-- Entschluesselt Nachricht mit Super-Absteigender Liste (reverse private sequence)
decryptHelp :: [Integer] -> Integer -> [Integer]
decryptHelp [k] c    | c - k == 0 = [1]
                     | c     == 0 = [0]
                     | otherwise  = error "Fehlerhafte Message - a"
decryptHelp (k:ks) c | c - k >= 0 = (decryptHelp ks (c - k)) ++ [1]
                     | otherwise  = (decryptHelp ks c) ++ [0]

decrypt :: PrivateKey -> Integer -> [Integer]
decrypt k c = decryptHelp (reverse (getSecretSequence k)) (((c * (multInverse (getR k) (getQ k))) `mod` (getQ k)))
