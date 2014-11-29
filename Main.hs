module Main where

import qualified System.Environment as SE
import qualified Knapsack as KS
import qualified Keys as K
import qualified Tests as T
import Crack

main = SE.getArgs >>= handleArgs >>= putStrLn

handleArgs :: [String] -> IO String
handleArgs ["encrypt", publicKey]  = Main.encrypt publicKey
handleArgs ["decrypt", privateKey] = Main.decrypt privateKey
handleArgs ["create", fileName]    = Main.create fileName
--handleArgs ["runtests"]            = return . show $ T.test
--handleArgs ["shell"]               = return "foo"
handleArgs ["crack", publicKey]    = Main.crack publicKey
handleArgs _                       = return $ unlines options

options = 
  [ "Usage: "
  , ""
  , "Knapsack encrypt <public key file>  -- encrypt message from stdin"
  , "Knapsack decrypt <private key file> -- decrypt message from stdin"
  , "Knapsack create <keypair name>      -- creates a public/private key pair"
  , "Knapsack crack <public key file>    -- crack message from stdin"
  , "Knapsack runtests                   -- runs tests for the different modules"
  , "Knapsack shell                      -- starts an interactive shell"
  , ""
  , "Example usage:"
  , "$ ./Knapsack create test"
  , "$ echo \"Ultra secret message\" | ./Knapsack encrypt test.pub > c"
  , "$ cat c | ./Knapsack decrypt test.private"
  ]

encrypt :: FilePath -> IO String
encrypt p = do
  k <- K.loadPublicKey p
  s <- getContents
  let c = show (KS.encryptString k s)
  return c

decrypt :: FilePath -> IO String
decrypt p = do
  k <- K.loadPrivateKey p
  s <- getContents
  let l = read s :: [Integer]
  return (KS.decryptString k l)

create :: String -> IO String
create name = do
  let private = K.genPrivateKey 8
  let privateName = name ++ ".private"
  K.savePrivateKey private privateName
  let public = K.genPublicKey private
  let publicName = name ++ ".pub"
  K.savePublicKey public publicName
  let s = unwords ["Create two key files:", privateName, publicName]
  return s

crack :: String -> IO String
crack p = do
  k <- K.loadPublicKey p
  return "foo"
