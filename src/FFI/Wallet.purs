-- | FFI for CIP-30 wallet interaction via Eternl.
module FFI.Wallet
  ( WalletApi
  , detectEternl
  , enableWallet
  , signTx
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)

-- | Opaque handle to the CIP-30 wallet API.
foreign import data WalletApi :: Type

-- | Check whether the Eternl extension is available.
foreign import detectEternl :: Effect Boolean

foreign import enableWalletImpl
  :: (Error -> Effect Unit)
  -> (WalletApi -> Effect Unit)
  -> Effect Unit

foreign import signTxImpl
  :: WalletApi
  -> String
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

-- | Enable the Eternl wallet and obtain an API handle.
enableWallet :: Aff WalletApi
enableWallet = makeAff \cb -> do
  enableWalletImpl
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
