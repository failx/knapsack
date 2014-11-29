module Main where

import qualified System.Environment as SE
import qualified Knapsack as KS
import qualified Keys as K
--import qualified Tests as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Crack

main = SE.getArgs >>= handleArgs >>= putStrLn

handleArgs :: [String] -> IO String
--handleArgs ["encrypt", publicKey]  = fmap BS8.unpack (Main.encrypt publicKey)
--handleArgs ["decrypt", privateKey] = fmap BS8.unpack (Main.decrypt privateKey)
--handleArgs ["create", name]        = create name
--handleArgs ["runtests"]            = return . show $ T.test
--handleArgs ["shell"]               = return "foo"
--handleArgs ["crack", publicKey]    = return "foo"
handleArgs _                       = return $ unlines options

options = 
  [ "clks usage: "
  , "clks encrypt <public key file>    -- reads from stdin and encrypts to stdout"
  , "clks decrypt <private key file>   -- reads from stdin and decrypts to stdout"
  , "clks create <keypair name>        -- creates a public/private key pair"
  , "clks runtests                     -- runs tests for the different modules"
  , "clks shell                        -- starts an interactive shell"
  , "clks crack <public key file>      -- reads from stdin and cracks to stdout"
  ]

--encrypt :: FilePath -> IO BS.ByteString
--encrypt p = do
--  string <- BS.getContents
--  key <- K.loadPublicKey p
--  return (KS.encryptString key string)
--
--decrypt :: FilePath -> IO BS.ByteString
--decrypt p = do
--  string <- BS.getContents
--  key <- K.loadPrivateKey p
--  return (KS.decryptString key string)
--
--create :: String -> IO String
--create name = do
--  let private = K.genPrivateKey 8
--  _ <- K.savePrivateKey private (name ++ ".private")
--  let public = K.genPublicKey private
--  _ <- K.savePublicKey public (name ++ ".pub")
--  return "Created key pair."
