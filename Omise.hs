{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}

module Omise
  ( Card (..)
  , TokenReq (..)
  , ChargeReq (..)
  , createToken
  , createCharge
  , putResponse
  ) where

import Prelude hiding (putStrLn)
import GHC.Generics (Generic)
import Data.Aeson
  ( ToJSON
  , Value(..)
  , Object
  , toEncoding
  , (.=)
  , pairs
  , decode
  )
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Function ((&))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Simple
  ( Response
  , setRequestHeaders
  , setRequestBasicAuth
  , setRequestBodyJSON
  , httpLBS
  , getResponseBody
  )
import Data.ByteString.Lazy.Char8 (putStrLn)
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.HashMap.Strict as M (lookup)

vaultBaseEnv :: MonadIO m => m String
vaultBaseEnv = liftIO $ getEnv "VAULT_BASE_URL"

apiBaseEnv :: MonadIO m => m String
apiBaseEnv = liftIO $ getEnv "API_BASE_URL"

livePKeyEnv :: MonadIO m => m String
livePKeyEnv = liftIO $ getEnv "PKEY"

liveSKeyEnv :: MonadIO m => m String
liveSKeyEnv = liftIO $ getEnv "SKEY"

data Card = Card
  { cardName            :: Text
  , cardNumber          :: Text
  , cardExpirationMonth :: Int
  , cardExpirationYear  :: Int
  , cardCity            :: Text
  , cardPostalCode      :: Int
  , cardSecurityCode    :: Int
  } deriving (Generic, Eq, Show)

instance ToJSON Card where
  toEncoding Card {..} =
    pairs $ "name"             .= cardName
         <> "number"           .= cardNumber
         <> "expiration_month" .= cardExpirationMonth
         <> "expiration_year"  .= cardExpirationYear
         <> "city"             .= cardCity
         <> "postal_code"      .= cardPostalCode
         <> "security_code"    .= cardSecurityCode

data TokenReq = TokenReq
  { tokenReqCard :: Card
  } deriving (Generic, Eq, Show)

instance ToJSON TokenReq where
  toEncoding TokenReq{..} =
    pairs $ "card" .= tokenReqCard

data ChargeReq = ChargeReq
  { chargeReqDescription :: Text
  , chargeReqAmount :: Int
  , chargeReqCurrency :: Text
  , chargeReqCard :: Text
  , chargeReqReturnURI :: Maybe Text
  } deriving (Generic, Eq, Show)

instance ToJSON ChargeReq where
  toEncoding ChargeReq{..} =
    pairs $ "description" .=  chargeReqDescription
         <> "amount"      .=  chargeReqAmount
         <> "currency"    .=  chargeReqCurrency
         <> "card"        .=  chargeReqCard
         <> "return_uri"  .=? chargeReqReturnURI
    where _ .=? Nothing = mempty -- omitNothing
          k .=? v       = k .= v

postAPI :: (MonadIO m, ToJSON a) => String -> String -> a -> m (Response LBS.ByteString)
postAPI url key body = fromString ("POST " ++ url)
                    & setRequestHeaders [("Accept", "application/json"), ("Content-Type", "application/json")]
                    & setRequestBasicAuth (fromString key) ""
                    & setRequestBodyJSON body
                    & httpLBS

extractTokenID :: Response LBS.ByteString -> Text
extractTokenID respLBS = tokenID
  where String tokenID = fromJust $ M.lookup "id" objBody
        objBody = fromJust $ decodedBody
        decodedBody = decode $ getResponseBody respLBS :: Maybe Object

createToken :: MonadIO m => TokenReq -> m (Response LBS.ByteString)
createToken req = do
  vaultBase <- vaultBaseEnv
  livePKey <- livePKeyEnv
  req & postAPI (vaultBase <> "/tokens") livePKey

createCharge :: MonadIO m => ChargeReq -> Response LBS.ByteString -> m (Response LBS.ByteString)
createCharge req stdin = do
  apiBase <- apiBaseEnv
  liveSKey <- liveSKeyEnv
  let tokenID = extractTokenID stdin
  req { chargeReqCard = tokenID } & postAPI (apiBase <> "/charges") liveSKey

putResponse :: Response LBS.ByteString -> IO ()
putResponse respLBS = getResponseBody respLBS & putStrLn
