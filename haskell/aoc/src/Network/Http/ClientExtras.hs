module Network.Http.ClientExtras (runRequest, Request (..), Response (..)) where

import Control.Arrow (second)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSUTF
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Network.HTTP.Types (HeaderName)
import qualified Network.HTTP.Types.Status as Http

data Request = Request
  { requestMethod :: String,
    requestHeaders :: [(HeaderName, String)],
    requestUrl :: String
  }

data Response = Response
  { responseStatus :: Int,
    responseBody :: String
  }

-- | VERY simple HTTP request client. Basically zero configuration
-- and no error handling.
runRequest :: Request -> IO Response
runRequest r = do
  manager <- Http.newManager Http.tlsManagerSettings
  req <- Http.parseRequest (requestUrl r)
  let req' =
        req
          { Http.method = BS.pack (requestMethod r),
            Http.requestHeaders = second BS.pack <$> requestHeaders r
          }
  resp <- Http.httpLbs req' manager
  return $
    Response
      { responseStatus = Http.statusCode $ Http.responseStatus resp,
        responseBody = BSUTF.toString $ Http.responseBody resp
      }
