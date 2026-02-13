-- | FFI for CIP-30 wallet interaction.
module FFI.Wallet
  ( WalletApi
  , WalletEntry
  , detectWallets
  , enableWalletByKey
  , signTx
  , getNetworkId
  , getBalance
  , getUtxos
  , getUsedAddresses
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , signData
  , submitTx
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)

-- | Opaque handle to the CIP-30 wallet API.
foreign import data WalletApi :: Type

-- | A detected CIP-30 wallet entry.
type WalletEntry =
  { key :: String
  , name :: String
  , icon :: String
  }

-- | Detect all available CIP-30 wallets.
foreign import detectWallets
  :: Effect (Array WalletEntry)

foreign import enableWalletByKeyImpl
  :: String
  -> (Error -> Effect Unit)
  -> (WalletApi -> Effect Unit)
  -> Effect Unit

foreign import signTxImpl
  :: WalletApi
  -> String
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import getNetworkIdImpl
  :: WalletApi
  -> (Error -> Effect Unit)
  -> (Int -> Effect Unit)
  -> Effect Unit

foreign import getBalanceImpl
  :: WalletApi
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import getUtxosImpl
  :: WalletApi
  -> (Error -> Effect Unit)
  -> (Array String -> Effect Unit)
  -> Effect Unit

foreign import getUsedAddressesImpl
  :: WalletApi
  -> (Error -> Effect Unit)
  -> (Array String -> Effect Unit)
  -> Effect Unit

foreign import getUnusedAddressesImpl
  :: WalletApi
  -> (Error -> Effect Unit)
  -> (Array String -> Effect Unit)
  -> Effect Unit

foreign import getChangeAddressImpl
  :: WalletApi
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import getRewardAddressesImpl
  :: WalletApi
  -> (Error -> Effect Unit)
  -> (Array String -> Effect Unit)
  -> Effect Unit

foreign import signDataImpl
  :: WalletApi
  -> String
  -> String
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import submitTxImpl
  :: WalletApi
  -> String
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

-- | Enable a wallet by its key and obtain an API
-- | handle.
enableWalletByKey :: String -> Aff WalletApi
enableWalletByKey key = makeAff \cb -> do
  enableWalletByKeyImpl key
    (\err -> cb (Left err))
    (\api -> cb (Right api))
  pure mempty

-- | Sign a transaction CBOR hex with partial signing.
signTx :: WalletApi -> String -> Aff String
signTx api cbor = makeAff \cb -> do
  signTxImpl api cbor
    (\err -> cb (Left err))
    (\wit -> cb (Right wit))
  pure mempty

-- | Get the network ID (0 = testnet, 1 = mainnet).
getNetworkId :: WalletApi -> Aff Int
getNetworkId api = makeAff \cb -> do
  getNetworkIdImpl api
    (\err -> cb (Left err))
    (\nid -> cb (Right nid))
  pure mempty

-- | Get the wallet balance as CBOR hex.
getBalance :: WalletApi -> Aff String
getBalance api = makeAff \cb -> do
  getBalanceImpl api
    (\err -> cb (Left err))
    (\bal -> cb (Right bal))
  pure mempty

-- | Get all UTXOs as CBOR hex strings.
getUtxos :: WalletApi -> Aff (Array String)
getUtxos api = makeAff \cb -> do
  getUtxosImpl api
    (\err -> cb (Left err))
    (\us -> cb (Right us))
  pure mempty

-- | Get all used addresses as hex strings.
getUsedAddresses :: WalletApi -> Aff (Array String)
getUsedAddresses api = makeAff \cb -> do
  getUsedAddressesImpl api
    (\err -> cb (Left err))
    (\as -> cb (Right as))
  pure mempty

-- | Get all unused addresses as hex strings.
getUnusedAddresses :: WalletApi -> Aff (Array String)
getUnusedAddresses api = makeAff \cb -> do
  getUnusedAddressesImpl api
    (\err -> cb (Left err))
    (\as -> cb (Right as))
  pure mempty

-- | Get the change address as hex string.
getChangeAddress :: WalletApi -> Aff String
getChangeAddress api = makeAff \cb -> do
  getChangeAddressImpl api
    (\err -> cb (Left err))
    (\a -> cb (Right a))
  pure mempty

-- | Get reward/stake addresses as hex strings.
getRewardAddresses :: WalletApi -> Aff (Array String)
getRewardAddresses api = makeAff \cb -> do
  getRewardAddressesImpl api
    (\err -> cb (Left err))
    (\as -> cb (Right as))
  pure mempty

-- | Sign arbitrary data (CIP-8). Returns JSON string
-- | of the DataSignature {signature, key}.
signData
  :: WalletApi -> String -> String -> Aff String
signData api addr payload = makeAff \cb -> do
  signDataImpl api addr payload
    (\err -> cb (Left err))
    (\sig -> cb (Right sig))
  pure mempty

-- | Submit a fully-signed transaction CBOR hex.
-- | Returns the transaction hash.
submitTx :: WalletApi -> String -> Aff String
submitTx api cbor = makeAff \cb -> do
  submitTxImpl api cbor
    (\err -> cb (Left err))
    (\h -> cb (Right h))
  pure mempty
