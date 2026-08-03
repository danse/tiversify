module Main where

import Control.Exception (IOException, try)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Business (Account(..), Report(..), computeRates)
import qualified X

main :: IO ()
main = do
  args <- getArgs
  case args of
    [handle] -> run handle
    _ -> do
      prog <- getProgName
      putStrLn ("Usage: " <> prog <> " <handle>")

run :: String -> IO ()
run handle = do
  result <- try (work handle) :: IO (Either IOException ())
  case result of
    Left err -> putStrLn ("error: " <> show err) >> exitFailure
    Right () -> pure ()
  where
    work h = do
      accounts <- X.followed (Account h)
      outputs <- mapM X.dailyOutput accounts
      putStr (show (Report (computeRates outputs)))
