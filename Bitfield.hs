module Bitfield where

-- Converts one Byte into a [0,1] Bitfield
byteToBin :: Int -> Int -> [Int]
byteToBin c 8 = []
byteToBin c n =  byteToBin (c `div` 2) (n + 1) ++ [c `mod` 2]

-- Converts a [0,1] Bitfield into a Byte
binToByte :: [Int] -> Int -> Int
binToByte [] _ = 0
binToByte (x:xs) n = (x * 2^n) + (binToByte xs (n - 1))

-- Converts a Char into a a [0,1] Bitfield
charToBin :: Char -> [Int]
charToBin c = byteToBin (fromEnum c) 0

-- Converts a [0,1] Bitfield into a Char
binToChar :: [Int] -> Char
binToChar x = toEnum (binToByte x 7) :: Char

-- Converts a String into a [0,1] Bifield
stringToBin :: String -> [Int]
stringToBin [] = []
stringToBin (x:xs) = charToBin x ++ stringToBin xs

-- Converts a [0,1] Bitfield into a String
binToString :: [Int] -> String
binToString [] = []
binToString x 
            | length x `mod` 8 /= 0 = error "Bitfield has to be multible of 8"
            | otherwise             = binToChar (take 8 x) : binToString (drop 8 x)
