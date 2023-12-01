module CLI (main) where

import Advent.Web (downloadInput)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just cmd -> run cmd
    Nothing -> putStrLn "invalid arguments expected 'new-day <day>' or 'run-day <day>'"

run :: Command -> IO ()
run (NewDay day) = createDay 2023 day
run (RunDay day) = putStrLn $ "run day " ++ show day

createDay :: Int -> Int -> IO ()
createDay year day = do
  session <- readFile "data/session_key.txt"
  content <- downloadInput year day session
  print content

data Command
  = NewDay Int
  | RunDay Int
  deriving (Show)

-- | Parse the command line arguments into a command
-- >>> parseArgs ["new-day", "1"]
-- Just (NewDay 1)
-- >>> parseArgs ["run-day", "12"]
-- Just (RunDay 12)
-- >>> parseArgs ["new"]
-- Nothing
parseArgs :: [String] -> Maybe Command
parseArgs ["new-day", day] = Just $ NewDay (read day)
parseArgs ["run-day", day] = Just $ RunDay (read day)
parseArgs _ = Nothing
