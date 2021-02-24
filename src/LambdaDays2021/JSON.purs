module LambdaDays2021.JSON where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe (..))
import Data.Argonaut.Core as JSON
import Data.Argonaut.Parser as AP
import Data.Codec.Argonaut as CA
import Data.Codec as DC
import Data.Codec ((>~>))
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CAS

data JsonError = ParseError String | DecodeError String

instance showJsonError :: Show JsonError where
  show (ParseError s)  = "ParseError: "  <> s
  show (DecodeError s) = "DecodeError: " <> s

data SubscriptionStatus = Trialing
                        | Active
                        | Incomplete
                        | IncompleteExpired
                        | PastDue
                        | Canceled
                        | Unpaid

instance showSubscriptionStatus :: Show SubscriptionStatus where
  show Trialing          = "trialing"
  show Active            = "active"
  show Incomplete        = "incomplete"
  show IncompleteExpired = "incomplete_expired"
  show PastDue           = "past_due"
  show Canceled          = "canceled"
  show Unpaid            = "unpaid"

subscriptionStatusCodec :: CA.JsonCodec SubscriptionStatus
subscriptionStatusCodec = CAS.enumSum encoder decoder
 where encoder :: SubscriptionStatus -> String
       encoder = show

       decoder :: String -> Maybe SubscriptionStatus
       decoder "trialing"           = Just Trialing
       decoder "active"             = Just Active
       decoder "incomplete"         = Just Incomplete
       decoder "incomplete_expired" = Just IncompleteExpired
       decoder "past_due"           = Just PastDue
       decoder "canceled"           = Just Canceled
       decoder "unpaid"             = Just Unpaid
       decoder _                    = Nothing

-- ** InvoiceSatus
data InvoiceStatus = Paid
                   | Open

instance showInvoiceStatus :: Show InvoiceStatus where
  show Paid = "paid"
  show Open = "open"

invoiceStatusCodec :: CA.JsonCodec InvoiceStatus
invoiceStatusCodec = CAS.enumSum encoder decoder
 where encoder :: InvoiceStatus -> String
       encoder = show

       decoder :: String -> Maybe InvoiceStatus
       decoder "paid" = Just Paid
       decoder "open" = Just Open
       decoder _      = Nothing

type Invoice
  = { id :: String
    , status :: InvoiceStatus
    }

type SubscriptionEvent
  = { id :: String
    , object :: String
    , status :: SubscriptionStatus
    , latest_invoice :: Invoice
    }

-- ** Codecs

invoiceCodec :: CA.JsonCodec Invoice
invoiceCodec = CA.object "Invoice" $
                 CAR.record { id: CA.string, status: invoiceStatusCodec }

subscriptionEventCodec :: CA.JsonCodec SubscriptionEvent
subscriptionEventCodec = CA.object "SubscriptionEvent" $ CAR.record
        { id: CA.string
        , object: CA.string
        , status: subscriptionStatusCodec
        , latest_invoice: invoiceCodec }

objectCodec :: CA.JsonCodec { object :: String }
objectCodec = CA.object "Event" $ CAR.record { object: CA.string }

-- ** Examples

validSubscriptionJson :: String
validSubscriptionJson = """
 {
  "id": "sub_1ELI8bClCIKljWvsvK36TXlC",
  "object": "subscription",
  "status": "active",
  "latest_invoice": {
    "id": "in_EmGqfJMYy3Nt9M",
    "status": "paid"
  }
}
"""

invalidSubscriptionJson1 :: String
invalidSubscriptionJson1 = """
 {
  "id": "sub_1ELI8bClCIKljWvsvK36TXlC",
  "object": "subscription",
  "status": "foo",
  "latest_invoice": {
    "id": "in_EmGqfJMYy3Nt9M",
    "status": "paid"
  }
}
"""

invalidSubscriptionJson2 :: String
invalidSubscriptionJson2 = """
 {
  "id": 1234,
  "object": "subscription",
  "status": "unpaid",
  "latest_invoice": {
    "id": "in_EmGqfJMYy3Nt9M",
    "status": "open"
  }
}
"""

invalidSubscriptionJson3 :: String
invalidSubscriptionJson3 = """
 {
  "event_id": "abcdef",
  "object": "subscription",
  "status": "unpaid",
  "latest_invoice": {
    "id": "in_EmGqfJMYy3Nt9M",
    "status": "open"
  }
}
"""

invalidSubscriptionJson4 :: String
invalidSubscriptionJson4 = """
 {
  "object": "subscription",
  "status": "unpaid",
  "latest_invoice": {
    "id": "in_EmGqfJMYy3Nt9M",
    "status": "open"
  }
}
"""

-- Migrations / Patches

-- >>> parse invalidSubscriptionJson3 >>= decode patchedCodec1
patchedCodec1
  = CAM.renameField "event_id" "id"
    >~> subscriptionEventCodec

-- >>> parse invalidSubscriptionJson4 >>= decode patchedCodec2
patchedCodec2
  = CAM.addDefaultField "id" JSON.jsonEmptyString
    >~> subscriptionEventCodec

-- Usage

parse :: String -> Either JsonError JSON.Json
parse str = lmap toParseError (AP.jsonParser str)
  where toParseError = ParseError

decode :: forall a
        . CA.JsonCodec a
       -> JSON.Json
       -> Either JsonError a
decode codec json = lmap toDecodeError dec
  where toDecodeError = DecodeError <<< show
        dec = DC.decode codec json

encode
  :: forall a
   . CA.JsonCodec a
  -> a
  -> Either JsonError JSON.Json
encode codec = pure <<< DC.encode codec

stringify
  :: JSON.Json
  -> Either JsonError String
stringify = pure <<< JSON.stringify

roundtrip
  :: forall a
   . CA.JsonCodec a
  -> String
  -> Either JsonError String
roundtrip codec json = parse json >>= decode codec >>= encode codec >>= stringify
