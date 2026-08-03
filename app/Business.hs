{-# LANGUAGE NoFieldSelectors #-}
module Business
  (tiversify)
where

newtype Account = Account String

newtype Report = Report [Rate]

instance Show Report where
  show (Report rates) =
    let s = sortOn (Down . (.percent)) rates
        d (Rate a p) = show a <> " sent " <> show p <> " of the messages"
        rest = d <$> take 4 s
    in unlines ("Accounts with most messages: ": rest)

newtype Output = Output Account Int

newtype Rate = Rate { account::Account, percent::Percent }

-- | Always use the `percent` constructor for this
newtype Percent = Percent Float

percent :: Float -> Either String Percent
percent f
  | 0 <= f && f <= 1 = Right $ Percent f
  | otherwise = Left (show f <> " is not in a percent range")

-- | Fetches the number of messages sent by an account in the past 24
-- hours
dailyOutput :: Account -> IO Output

computeRates :: [Output] -> [Rate]
computeRates os =
  let t = _total os
      f :: Int -> Output -> Rate
      f t (Output handle count) =
        Rate handle (percent $ fromInt count/fromInt t)
  in (f t) <$> os

tiversify :: String -> IO String
tiversify handle = do
  d <- followed (Account handle)
  outputs <- mapM dailyOutput d
  let rates = computeRates outputs
  pure . show . Report $ rates
