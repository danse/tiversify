module Main where

import Control.Exception (IOException, try)
import System.Exit (exitFailure)

import Options.Applicative
  ( Parser
  , argument
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , short
  , str
  , switch
  , (<**>)
  )

import Business (Account(..), Report(..), computeRates)
import qualified X

data Options = Options
  { optVerbose :: Bool
  , optHandle :: String
  }

options :: Parser Options
options =
  Options
    <$> switch (long "verbose" <> short 'v' <> help "Log each API call to stderr")
    <*> argument str (metavar "HANDLE" <> help "X account whose follows to analyze")

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) (fullDesc <> progDesc "Report how often each followed account posts on X" <> header "tiversify"))
  run (optVerbose opts) (optHandle opts)

run :: Bool -> String -> IO ()
run verbose handle = do
  mgr <- X.manager
  result <- try (work mgr verbose handle) :: IO (Either IOException ())
  case result of
    Left err -> putStrLn ("error: " <> show err) >> exitFailure
    Right () -> pure ()
  where
    work m v h = do
      accounts <- X.followed m v (Account h)
      outputs <- mapM (X.dailyOutput m v) accounts
      putStr (show (Report (computeRates outputs)))
