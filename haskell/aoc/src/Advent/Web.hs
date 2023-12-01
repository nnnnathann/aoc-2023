module Advent.Web (downloadInput) where

import Network.Http.ClientExtras (Request (..), responseBody, runRequest)

-- | Download the input for a given year and day, using the session cookie
downloadInput :: Int -> Int -> String -> IO String
downloadInput year day session = do
  response <-
    runRequest $
      Request
        { requestMethod = "GET",
          requestHeaders = [("Cookie", "session=" ++ session)],
          requestUrl = mkInputUrl year day
        }
  return $ responseBody response

-- | Construct the URL for the input for a given year and day
mkInputUrl :: Int -> Int -> String
mkInputUrl year day = "https://adventofcode.com/" ++ show year ++ "/day/" ++ show day ++ "/input"