{-# LANGUAGE OverloadedStrings #-}

module Main where

import OmiseClient

main :: IO ()
main = do
  let
    tokenReq = TokenReq
      { tokenReqCard = Card
        { cardName            = "Somchai Prasert"
        , cardNumber          = "0123456789012345"
        , cardExpirationMonth = 10
        , cardExpirationYear  = 2018
        , cardCity            = "Bangkok"
        , cardPostalCode      = 10320
        , cardSecurityCode    = 123
        }
      }
    chargeReq = ChargeReq
      { chargeReqDescription = "Example Charge"
      , chargeReqAmount      = 100000
      , chargeReqCurrency    = "thb"
      , chargeReqCard        = ""
      , chargeReqReturnURI   = Nothing
      }

  createToken tokenReq >>= createCharge chargeReq >>= putResponse
