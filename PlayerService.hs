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
    let getParam n v = maybe v id $ lookup n params
        action       = getParam (B.pack "action") (B.pack "version")
        state        = parseJSON $ getParam (B.pack "game_state") (B.pack "{}") :: Either String Object
        withState f  = either badRequest f state
    case B.unpack action of
      "check"       -> sayVersion
      "version"     -> sayVersion
      "bet_request" -> withState $ \s -> betRequest s >>= ok . L.pack . show
      "showdown"    -> withState $ \s -> showdown s >> ok (L.pack "")
      _             -> badRequest "unknown action"
  else sayVersion
  where
    parseJSON = eitherDecode' . fromStrict
    sayVersion = ok $ L.pack $ version
    ok = send status200
    badRequest = send status400 . append (L.pack "Bad request: ") . L.pack
    send status = respond . responseLBS status headers
    headers = [ (hServer, B.pack "Haskell Lean Poker Player")
              , (hContentType, B.pack "text/plain") ]

