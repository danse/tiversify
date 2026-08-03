{-# LANGUAGE OverloadedStrings #-}

-- | X-specific calls: authenticating with OAuth 1.0a user context and
-- querying the X API v2.
module X
  ( followed
  , dailyOutput
  ) where

import Business (Account(..), Output(..))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Crypto.Hash as CH
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Time
  ( UTCTime
  , addUTCTime
  , defaultTimeLocale
  , formatTime
  , getCurrentTime
  )
import Network.HTTP.Client
  ( Request
  , defaultRequest
  , host
  , httpLbs
  , newManager
  , path
  , port
  , queryString
  , requestHeaders
  , responseBody
  , responseStatus
  , secure
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusIsSuccessful)
import System.Environment (lookupEnv)

apiHost :: BS.ByteString
apiHost = "api.x.com"

apiBase :: T.Text
apiBase = "https://api.x.com"

-- | Maximum number of paginated pages fetched per timeline.
maxPages :: Int
maxPages = 10

-- | Credentials for OAuth 1.0a user context, read from the environment.
data Creds = Creds
  { consumerKey :: T.Text
  , consumerSecret :: T.Text
  , accessToken :: T.Text
  , accessSecret :: T.Text
  }

loadCreds :: IO Creds
loadCreds = do
  ck <- env "X_API_KEY"
  cs <- env "X_API_SECRET"
  at <- env "X_ACCESS_TOKEN"
  as <- env "X_ACCESS_SECRET"
  pure (Creds ck cs at as)
  where
    env var = do
      value <- lookupEnv var
      case value of
        Just v | not (null v) -> pure (T.pack v)
        _ -> fail ("missing environment variable: " <> var)

-- | OAuth 1.0a per-request parameters.
data OAuth = OAuth
  { oConsumerKey :: T.Text
  , oNonce :: T.Text
  , oTimestamp :: T.Text
  , oAccessToken :: T.Text
  }

oauthParams :: OAuth -> [(T.Text, T.Text)]
oauthParams o =
  [ ("oauth_consumer_key", oConsumerKey o)
  , ("oauth_nonce", oNonce o)
  , ("oauth_signature_method", "HMAC-SHA1")
  , ("oauth_timestamp", oTimestamp o)
  , ("oauth_token", oAccessToken o)
  , ("oauth_version", "1.0")
  ]

newOAuth :: UTCTime -> Creds -> OAuth
newOAuth now creds =
  OAuth
    { oConsumerKey = consumerKey creds
    , oNonce = nonce now
    , oTimestamp = timestamp now
    , oAccessToken = accessToken creds
    }
  where
    nonce = T.pack . filter Char.isAlphaNum . show
    timestamp = T.pack . formatTime defaultTimeLocale "%s"

-- | RFC 3986 percent-encoding, as used by OAuth 1.0a.
pctEncode :: T.Text -> T.Text
pctEncode = TE.decodeUtf8 . BS.concatMap enc . TE.encodeUtf8
  where
    enc b
      | isUnreserved b = BS.singleton b
      | otherwise = BS.pack (37 : hexWord8 b)
    isUnreserved b =
      (b >= 48 && b <= 57)
      || (b >= 65 && b <= 90)
      || (b >= 97 && b <= 122)
      || b `elem` [45, 46, 95, 126]
    hexWord8 b =
      let hi = b `div` 16
          lo = b `mod` 16
      in [digits !! fromIntegral hi, digits !! fromIntegral lo]
    digits = [48 .. 57] ++ [65 .. 70]

encodePairs :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
encodePairs = map (\(k, v) -> (pctEncode k, pctEncode v))

renderPairs :: [(T.Text, T.Text)] -> T.Text
renderPairs = T.intercalate "&" . map (\(k, v) -> k <> "=" <> v)

-- | OAuth 1.0a HMAC-SHA1 request signature.
signature :: Creds -> T.Text -> T.Text -> [(T.Text, T.Text)] -> OAuth -> T.Text
signature creds method baseUrl queryParams oa =
  let allParams = List.sort (encodePairs (queryParams <> oauthParams oa))
      baseString =
        method <> "&"
        <> pctEncode baseUrl <> "&"
        <> pctEncode (renderPairs allParams)
      signingKey =
        pctEncode (consumerSecret creds) <> "&"
        <> pctEncode (accessSecret creds)
  in TE.decodeUtf8 (B64.encode (hmacSha1 (TE.encodeUtf8 signingKey) (TE.encodeUtf8 baseString)))

hmacSha1 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacSha1 key msg =
  BA.convert (HMAC.hmacGetDigest (HMAC.hmac key msg :: HMAC.HMAC CH.SHA1))

renderAuth :: OAuth -> T.Text -> BS.ByteString
renderAuth oa sig =
  "OAuth "
  <> TE.encodeUtf8 (T.intercalate ", " (map render (encodePairs (oauthParams oa <> [("oauth_signature", sig)]))))
  where
    render (k, v) = k <> "=\"" <> v <> "\""

baseRequest :: T.Text -> BS.ByteString -> Request
baseRequest route query =
  defaultRequest
    { secure = True
    , host = apiHost
    , port = 443
    , path = TE.encodeUtf8 route
    , queryString = query
    }

-- | GET an X API endpoint, OAuth-signed, returning the response body.
signedGet :: Creds -> T.Text -> T.Text -> [(T.Text, T.Text)] -> IO LBS.ByteString
signedGet creds method route queryParams = do
  now <- getCurrentTime
  let oa = newOAuth now creds
      sig = signature creds method (apiBase <> route) queryParams oa
      authorization = renderAuth oa sig
      query = TE.encodeUtf8 (renderPairs (encodePairs queryParams))
      request =
        (baseRequest route query)
          { requestHeaders = [("Authorization", authorization)]
          }
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let status = responseStatus response
      body = responseBody response
  if statusIsSuccessful status
    then pure body
    else fail ("X API " <> show status <> ": " <> BSC.unpack (LBS.toStrict body))

lookupKey :: T.Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey key (Aeson.Object o) = KM.lookup (Key.fromText key) o
lookupKey _ _ = Nothing

lookupText :: T.Text -> Aeson.Value -> Maybe T.Text
lookupText key value = case value of
  Aeson.Object o -> case KM.lookup (Key.fromText key) o of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
  _ -> Nothing

decodeChecked :: LBS.ByteString -> IO Aeson.Value
decodeChecked body =
  case Aeson.eitherDecode body of
    Left err -> fail ("X: could not decode response: " <> err)
    Right value -> case lookupKey "errors" value of
      Just errs -> fail ("X API error: " <> show errs)
      Nothing -> pure value

-- | Numeric ID for an account.
lookupUserId :: Creds -> Account -> IO T.Text
lookupUserId creds (Account handle) = do
  let route = "/2/users/by/username/" <> pctEncode (T.pack handle)
  body <- signedGet creds "GET" route []
  value <- decodeChecked body
  case lookupText "id" =<< lookupKey "data" value of
    Just uid -> pure uid
    Nothing -> fail ("X: no user found for @" <> handle)

metaNextToken :: Aeson.Value -> Maybe T.Text
metaNextToken value = lookupText "next_token" =<< lookupKey "meta" value

followingPage :: Aeson.Value -> ([Account], Maybe T.Text)
followingPage value =
  let accounts = case lookupKey "data" value of
        Just (Aeson.Array arr) ->
          [ Account (T.unpack name)
          | Aeson.Object obj <- V.toList arr
          , Just name <- [lookupText "username" (Aeson.Object obj)]
          ]
        _ -> []
  in (accounts, metaNextToken value)

-- | Accounts followed by the given one.
followed :: Account -> IO [Account]
followed account = do
  creds <- loadCreds
  uid <- lookupUserId creds account
  let go token accounts = do
        let params =
              [("max_results", "1000")]
              <> maybe [] (\t -> [("pagination_token", t)]) token
        body <- signedGet creds "GET" ("/2/users/" <> uid <> "/following") params
        value <- decodeChecked body
        let (newAccounts, nextToken) = followingPage value
        case nextToken of
          Nothing -> pure (accounts <> newAccounts)
          Just t -> go (Just t) (accounts <> newAccounts)
  go Nothing []

-- | Messages sent by an account in the last 24 hours.
dailyOutput :: Account -> IO Output
dailyOutput account = do
  creds <- loadCreds
  uid <- lookupUserId creds account
  now <- getCurrentTime
  let since = addUTCTime (-24 * 60 * 60) now
  count <- countTweets creds uid since
  pure (Output account count)

countTweets :: Creds -> T.Text -> UTCTime -> IO Int
countTweets creds uid since = go Nothing 0 0
  where
    go token count pages
      | pages >= maxPages = pure count
      | otherwise = do
          let params =
                [("max_results", "100"), ("start_time", rfc3339 since)]
                <> maybe [] (\t -> [("pagination_token", t)]) token
          body <- signedGet creds "GET" ("/2/users/" <> uid <> "/tweets") params
          value <- decodeChecked body
          let n = tweetCount value
              next = metaNextToken value
          case next of
            Nothing -> pure (count + n)
            Just t -> go (Just t) (count + n) (pages + 1)

tweetCount :: Aeson.Value -> Int
tweetCount value = case lookupKey "data" value of
  Just (Aeson.Array arr) -> length (V.toList arr)
  _ -> 0

rfc3339 :: UTCTime -> T.Text
rfc3339 = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
