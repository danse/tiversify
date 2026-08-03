{-# LANGUAGE NoFieldSelectors #-}

module Business
  ( Account(..)
  , Output(..)
  , Report(..)
  , Rate(..)
  , Percent
  , percent
  , computeRates
  ) where

import Data.List (sortOn)
import Data.Ord (Down(..))

-- | A social media account, identified by handle.
newtype Account = Account String
  deriving (Eq, Ord, Show)

-- | The number of messages an account sent in the last 24 hours.
data Output = Output Account Int

-- | The share of messages each account contributes to the total.
data Rate = Rate { account :: Account, percent :: Percent }

-- | A report of the accounts that fill a feed the most.
newtype Report = Report [Rate]

instance Show Report where
  show (Report rates) =
    let s = sortOn (Down . ratePercent) rates
        line (Rate (Account handle) p) =
          handle <> " sent " <> show p <> " of the messages"
    in unlines ("Accounts with most messages: " : map line (take 4 s))

ratePercent :: Rate -> Percent
ratePercent (Rate _ p) = p

-- | Always use the `percent` constructor for this.
newtype Percent = Percent Float
  deriving (Eq, Ord)

instance Show Percent where
  show (Percent f) = show (round (f * 100) :: Int) <> "%"

-- | Smart constructor for a share of messages, in [0, 1].
percent :: Float -> Either String Percent
percent f
  | 0 <= f && f <= 1 = Right (Percent f)
  | otherwise = Left (show f <> " is not in a percent range")

-- | The share of messages each account contributes to the total.
computeRates :: [Output] -> [Rate]
computeRates outputs =
  let total = sum (outputCount <$> outputs)
      share c
        | total == 0 = 0
        | otherwise = fromIntegral c / fromIntegral total
      mkRate (Output account' count) =
        Rate account' (case percent (share count) of
          Right p -> p
          Left err -> error err)
  in mkRate <$> outputs
  where
    outputCount (Output _ c) = c
