{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NumericUnderscores         #-}

module Market.Onchain
    ( apiBuyScript
    , buyScriptAsShortBs
    , typedBuyValidator
    , Sale
    , buyValidator
    , nftDatum
    , apiOfferScript
    , offerScriptAsShortBs
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude
import PlutusTx.Ratio
import Ledger
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value
import           Plutus.V1.Ledger.Ada

import           Market.Types               (NFTSale(..), SaleAction(..))

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = getLovelace . fromValue

{-# INLINABLE nftDatum #-}
nftDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkOfferValidator #-}
mkOfferValidator :: PubKeyHash -> NFTSale -> SaleAction -> ScriptContext -> Bool
mkOfferValidator pkh nfts r ctx =
    case r of
        Buy   -> traceIfFalse "NFT not sent to buyer" checkNFTOut &&
                 traceIfFalse "Seller not paid" checkSellerOut &&
                 traceIfFalse "Fee not paid" checkMarketplaceFee &&
                 traceIfFalse "Royalities not paid" checkRoyaltyFee &&
                 traceIfFalse "Script is missing Ada" scriptInputAda
        Close -> traceIfFalse "No rights to perform this action" checkCloser
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = nToken nfts

    cs :: CurrencySymbol
    cs = nCurrency nfts

    buyer :: PubKeyHash
    buyer = nOwner nfts

    nftOwner :: PubKeyHash
    nftOwner = case txInfoSignatories info of
            [x] -> x
            _ -> error ()

    price :: Integer
    price = nPrice nfts

    checkNFTOut :: Bool
    checkNFTOut = valueOf (valuePaidTo info buyer) cs tn == 1

    marketplacePercent :: Integer
    marketplacePercent = 20

    marketplaceFee :: Ratio Integer
    marketplaceFee = max (1_000_000 % 1) (marketplacePercent % 1000 * fromInteger price)

    checkMarketplaceFee :: Bool
    checkMarketplaceFee
      = fromInteger (getLovelace (fromValue (valuePaidTo info pkh)))
      >= marketplaceFee

    royaltyFee :: Ratio Integer
    royaltyFee = if nRoyaltyPercent nfts > 0
      then max (1_000_000 % 1) (nRoyaltyPercent nfts % 1000 * fromInteger price)
      else fromInteger 0

    checkRoyaltyFee :: Bool
    checkRoyaltyFee
      = fromInteger (getLovelace (fromValue (valuePaidTo info $ nRoyalty nfts)))
      >= royaltyFee

    checkSellerOut :: Bool
    checkSellerOut
      =  fromInteger (getLovelace (fromValue (valuePaidTo info nftOwner)))
      >= ((fromInteger price - marketplaceFee) - royaltyFee)

    checkCloser :: Bool
    checkCloser = txSignedBy info buyer

    scriptValue :: Value
    scriptValue = maybe (error ()) (txOutValue . txInInfoResolved) $ findOwnInput ctx

    scriptInputAda :: Bool
    scriptInputAda = lovelaces scriptValue >= price

typedOfferValidator :: PubKeyHash -> Scripts.TypedValidator Sale
typedOfferValidator pkh = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkOfferValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


offerValidator :: PubKeyHash -> Validator
offerValidator = Scripts.validatorScript . typedOfferValidator

offerScript :: PubKeyHash -> Plutus.Script
offerScript = Ledger.unValidatorScript . offerValidator

offerScriptAsShortBs :: PubKeyHash -> SBS.ShortByteString
offerScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . offerScript

apiOfferScript :: PubKeyHash -> PlutusScript PlutusScriptV1
apiOfferScript
  = PlutusScriptSerialised
  . offerScriptAsShortBs


{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: PubKeyHash -> NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator pkh nfts r ctx =
    case r of
        Buy   -> traceIfFalse "NFT not sent to buyer" checkNFTOut &&
                 traceIfFalse "Seller not paid" checkSellerOut &&
                 traceIfFalse "Fee not paid" checkMarketplaceFee &&
                 traceIfFalse "Royalities not paid" checkRoyaltyFee
        Close -> traceIfFalse "No rights to perform this action" checkCloser &&
                 traceIfFalse "Close output invalid" checkCloseOut
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = nToken nfts

    cs :: CurrencySymbol
    cs = nCurrency nfts

    seller :: PubKeyHash
    seller = nOwner nfts

    sig :: PubKeyHash
    sig = case txInfoSignatories info of
            [x] -> x
            _ -> error ()

    price :: Integer
    price = nPrice nfts

    checkNFTOut :: Bool
    checkNFTOut = valueOf (valuePaidTo info sig) cs tn == 1

    marketplacePercent :: Integer
    marketplacePercent = 20

    marketplaceFee :: Ratio Integer
    marketplaceFee = max (1_000_000 % 1) (marketplacePercent % 1000 * fromInteger price)

    checkMarketplaceFee :: Bool
    checkMarketplaceFee
      = fromInteger (getLovelace (fromValue (valuePaidTo info pkh)))
      >= marketplaceFee

    royaltyFee :: Ratio Integer
    royaltyFee = if nRoyaltyPercent nfts > 0
      then max (1_000_000 % 1) (nRoyaltyPercent nfts % 1000 * fromInteger price)
      else fromInteger 0

    checkRoyaltyFee :: Bool
    checkRoyaltyFee
      = fromInteger (getLovelace (fromValue (valuePaidTo info $ nRoyalty nfts)))
      >= royaltyFee

    checkSellerOut :: Bool
    checkSellerOut
      =  fromInteger (getLovelace (fromValue (valuePaidTo info seller)))
      >= ((fromInteger price - marketplaceFee) - royaltyFee)

    checkCloser :: Bool
    checkCloser = txSignedBy info seller

    checkCloseOut :: Bool
    checkCloseOut = valueOf (valuePaidTo info seller) cs tn == 1


data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: PubKeyHash -> Scripts.TypedValidator Sale
typedBuyValidator pkh = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


buyValidator :: PubKeyHash -> Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyScript :: PubKeyHash -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: PubKeyHash -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: PubKeyHash -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs
