
import Control.Applicative ((<$>))
import Data.Aeson (eitherDecode', Object)
import Data.ByteString.Lazy (append, fromStrict)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L (pack, unpack)
import Network.Wai (Application, requestMethod, responseLBS)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (hServer, hContentType, status200, status400, methodPost)
import System.Environment (lookupEnv)

import Player

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  putStrLn $ "Listening on port " ++ show port ++ "..."
  run port handler

handler :: Application
handler request respond = if methodPost == requestMethod request
  then do
    (params, _) <- parseRequestBody lbsBackEnd request
    let getParam n v = maybe (B.pack v) id $ lookup (B.pack n) params
        action       = getParam "action" "version"
        state        = parseJSON $ getParam "game_state" "{}" :: Either String Object
        withState f  = either badRequest f state
    case B.unpack action of
      "check"       -> sayVersion
      "version"     -> sayVersion
      "bet_request" -> withState $ \s -> betRequest s >>= ok . show
      "showdown"    -> withState $ \s -> showdown s >> ok ""
      _             -> badRequest "unknown action"
  else sayVersion
  where
    parseJSON = eitherDecode' . fromStrict
    sayVersion = ok $ version
    ok = send status200
    badRequest = send status400 . ("Bad request: " ++)
    send status = respond . responseLBS status headers . L.pack
    headers = [ (hServer, B.pack "Haskell Lean Poker Player")
              , (hContentType, B.pack "text/plain") ]

