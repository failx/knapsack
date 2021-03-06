module Tests where
import Knapsack
import Keys
import Bitfield
import Test.QuickCheck

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

testBitfield = quickCheckWith stdArgs { maxSuccess = 5000 } ((\s -> (compare (binToString (stringToBin s)) s) == EQ) :: String -> Bool)
