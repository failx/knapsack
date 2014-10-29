import System.Random
import System.IO
import System.IO.Unsafe

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
getSecretSequence (PrivateKey _ _ _ sequence) = sequence

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
getPublicSequence (PublicKey _ sequence) = sequence

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
rndMin 0 = rndMinMax 0 $ 2^64
rndMin a = rndMinMax a $ (*) a 1024

-- Genereate a Super Increasing Sequence, Summe der vorgerigen Werte wird mit uebergeben
superinc_help :: Int -> Integer -> [Integer]
superinc_help 0 _ = []
superinc_help a b = let x = rndMin b
                   in  x : superinc_help (a - 1) (b + x)

-- Generiert Super Increasing Sequence mit Anzahl der Elemente, die generiert werden
superinc :: Int -> [Integer]
superinc a = superinc_help a 0

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
encrypt_help :: [Integer] -> [Integer] -> Integer
encrypt_help []     []     = 0
encrypt_help [k]    [t]    = (*) k t
encrypt_help (k:ks) (t:ts) = (+) ( (*) k t)  (encrypt_help ks ts)

-- Encrypt a List of 0,1 (Bitfield) with a PublicKey
encrypt :: PublicKey -> [Integer] -> Integer
encrypt k t 
            | (getPublicKeySize k) /= (length t) = error "Schluessegroesse ungleich Text"
            | otherwise                          = encrypt_help (getPublicSequence k) t
            
decrypt :: PrivateKey -> Integer -> [Integer]
decrypt _ _ = error "Nicht Implementiert"