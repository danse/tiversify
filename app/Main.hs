module Main where

import Control.Exception (IOException, try)
import System.Exit (exitFailure)

import Options.Applicative
  ( Parser
  , argument
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , str
  , switch
  , value
  , (<**>)
  )

import Business (Account(..), Report(..), computeRates)
import qualified X

data Options = Options
  { optVerbose :: Bool
  , optLimit :: Int
  , optHandle :: String
  }

options :: Parser Options
options =
  Options
    <$> switch (long "verbose" <> short 'v' <> help "Log each API call to stderr")
    <*> option auto (long "limit" <> short 'n' <> metavar "N" <> value 4 <> showDefault <> help "Number of accounts with most messages to show")
    <*> argument str (metavar "HANDLE" <> help "X account whose follows to analyze")

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) (fullDesc <> progDesc "Report how often each followed account posts on X" <> header "tiversify"))
  run (optVerbose opts) (optLimit opts) (optHandle opts)

run :: Bool -> Int -> String -> IO ()
run verbose limit handle = do
  mgr <- X.manager
  result <- try (work mgr verbose limit handle) :: IO (Either IOException ())
  case result of
    Left err -> putStrLn ("error: " <> show err) >> exitFailure
    Right () -> pure ()
  where
    work m v n h = do
      accounts <- X.followed m v (Account h)
      outputs <- mapM (X.dailyOutput m v) accounts
      putStr (show (Report (computeRates outputs) n))
