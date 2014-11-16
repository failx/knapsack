import System.Environment
import Knapsack

main = getArgs >>= handleArgs >>= putStrLn

handleArgs :: [String] -> IO String
handleArgs ["encrypt", publicKey]  = return "foo"
handleArgs ["decrypt", privateKey] = return "foo"
handleArgs ["create", name]        = return "foo"
handleArgs ["runtests"]            = return . show $ test
handleArgs ["shell"]               = return "foo"
handleArgs ["crack", publicKey]    = return "foo"
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
