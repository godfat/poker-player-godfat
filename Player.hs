module Player where

import Data.Aeson (Object)


version :: String
version = "Fixed Haskell folding player"

betRequest :: Object -> IO Int
betRequest gameState = return 0

showdown :: Object -> IO ()
showdown gameState = return ()

