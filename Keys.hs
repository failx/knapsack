module Keys where

import System.Random
import System.IO.Unsafe

-- Datenstruktur fuer eine privaten Schluessel
data PrivateKey = PrivateKey { privateKeySize :: Int
                             , privateKeyQ :: Integer
                             , privateKeyR :: Integer
                             , privateKeySequence :: [Integer]
                             } deriving (Show, Read)

-- Datenstruktur fuer eine oeffentlichen Schluessel
data PublicKey  = PublicKey { publicKeySize :: Int 
                            , publicKeySequence :: [Integer]
                            } deriving (Show, Read)

-- Save Private Key 
savePrivateKey :: PrivateKey -> FilePath -> IO()
savePrivateKey k p = writeFile p (show k)

-- Load Private Key
loadPrivateKey :: FilePath -> IO PrivateKey
loadPrivateKey p = fmap read (readFile p)
                   
-- Save Public Key
savePublicKey :: PublicKey -> FilePath -> IO()
savePublicKey k p = writeFile p (show k)

-- Load Public Key
loadPublicKey :: FilePath -> IO PublicKey
loadPublicKey p = fmap read (readFile p)

-- Euklidischer Algorithmus
euclid :: Integer -> Integer -> Integer
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

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
genPublicKey a = PublicKey (privateKeySize a)  (map (`mod` privateKeyQ a) $ map (privateKeyR a *) (privateKeySequence a))
