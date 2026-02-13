module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (trim, length)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import FFI.Clipboard (copyToClipboard)
import FFI.Wallet (WalletApi, detectEternl, enableWallet, signTx)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

-- | Status message severity.
data StatusKind = Info | Success | Error

-- | A status message with its severity.
type StatusMsg =
  { kind :: StatusKind
  , text :: String
  }

-- | Component state.
type State =
  { walletDetected :: Boolean
  , walletApi :: Maybe WalletApi
  , txCbor :: String
  , status :: Maybe StatusMsg
  , witness :: Maybe String
  , copyHint :: Boolean
  }

initialState :: forall i. i -> State
initialState _ =
  { walletDetected: false
  , walletApi: Nothing
  , txCbor: ""
  , status: Nothing
  , witness: Nothing
  , copyHint: false
  }

-- | Component actions.
data Action
  = Initialize
  | PollWallet Int
  | ConnectWallet
  | SetTxCbor String
  | SignTx
  | CopyWitness

component :: forall q i o. H.Component q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "container") ]
    [ HH.h1_ [ HH.text "Cardano Tx Signer" ]
    , HH.p
        [ HP.class_ (HH.ClassName "subtitle") ]
        [ HH.text
            "Sign transactions with Eternl via CIP-30"
        ]
    , renderWalletStatus state
    , renderCard state
    , renderStatus state
    , renderResult state
    ]

renderWalletStatus
  :: forall m. State -> H.ComponentHTML Action () m
renderWalletStatus state =
  HH.div
    [ HP.class_ (HH.ClassName "wallet-status") ]
    [ HH.span [ HP.class_ (HH.ClassName dotClass) ] []
    , HH.text statusText
    , if needsConnect then
        HH.button
          [ HP.class_ (HH.ClassName "btn-secondary")
          , HE.onClick \_ -> ConnectWallet
          ]
          [ HH.text "Connect" ]
      else
        HH.text ""
    ]
  where
  connected = isJust state.walletApi
  dotClass =
    if connected then "dot connected" else "dot"
  statusText
    | connected = "Eternl connected"
    | state.walletDetected = "Eternl detected"
    | otherwise = "Waiting for Eternl..."
  needsConnect =
    state.walletDetected && isNothing state.walletApi

renderCard
  :: forall m. State -> H.ComponentHTML Action () m
renderCard state =
  HH.div
    [ HP.class_ (HH.ClassName "card") ]
    [ HH.label_ [ HH.text "Transaction CBOR" ]
    , HH.textarea
        [ HP.placeholder "Paste transaction CBOR hex..."
        , HP.value state.txCbor
        , HE.onValueInput SetTxCbor
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "actions") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "btn-primary")
            , HP.disabled (not canSign)
            , HE.onClick \_ -> SignTx
            ]
            [ HH.text "Sign Transaction" ]
        ]
    ]
  where
  canSign =
    isJust state.walletApi
      && length (trim state.txCbor)
      > 0

renderStatus
  :: forall m. State -> H.ComponentHTML Action () m
renderStatus state = case state.status of
  Nothing ->
    HH.div [ HP.class_ (HH.ClassName "status") ] []
  Just msg ->
    HH.div
      [ HP.class_
          ( HH.ClassName
              ("status show " <> kindClass msg.kind)
          )
      ]
      [ HH.text msg.text ]

renderResult
  :: forall m. State -> H.ComponentHTML Action () m
renderResult state = case state.witness of
  Nothing ->
    HH.div
      [ HP.class_ (HH.ClassName "result-area") ]
      []
  Just wit ->
    HH.div
      [ HP.class_ (HH.ClassName "result-area show") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "card") ]
          [ HH.label_ [ HH.text "Witness" ]
          , HH.textarea
              [ HP.value wit
              , HP.readOnly true
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "actions") ]
              [ HH.button
                  [ HP.class_
                      (HH.ClassName "btn-secondary")
                  , HE.onClick \_ -> CopyWitness
                  ]
                  [ HH.text "Copy to Clipboard" ]
              ]
          , if state.copyHint then
              HH.div
                [ HP.class_ (HH.ClassName "copy-hint")
                ]
                [ HH.text "Copied!" ]
            else
              HH.text ""
          ]
      ]

kindClass :: StatusKind -> String
kindClass = case _ of
  Info -> "info"
  Success -> "success"
  Error -> "error"

handleAction
  :: forall o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> handleAction (PollWallet 20)

  PollWallet n -> do
    found <- liftEffect detectEternl
    if found then
      H.modify_ _ { walletDetected = true }
    else when (n > 0) do
      H.liftAff $ delay (Milliseconds 500.0)
      handleAction (PollWallet (n - 1))

  ConnectWallet -> do
    H.modify_ _
      { status = Just
          { kind: Info, text: "Connecting..." }
      }
    result <- H.liftAff $ attempt enableWallet
    case result of
      Right api -> H.modify_ _
        { walletApi = Just api
        , status = Just
            { kind: Success
            , text: "Wallet connected"
            }
        }
      Left err -> H.modify_ _
        { status = Just
            { kind: Error
            , text: "Connection failed: " <> show err
            }
        }

  SetTxCbor s -> H.modify_ _ { txCbor = s }

  SignTx -> do
    st <- H.get
    case st.walletApi of
      Nothing -> pure unit
      Just api -> do
        let cbor = trim st.txCbor
        when (length cbor > 0) do
          H.modify_ _
            { status = Just
                { kind: Info
                , text: "Signing..."
                }
            , witness = Nothing
            }
          result <-
            H.liftAff $ attempt (signTx api cbor)
          case result of
            Right wit -> H.modify_ _
              { witness = Just wit
              , status = Just
                  { kind: Success
                  , text: "Transaction signed"
                  }
              }
            Left err -> H.modify_ _
              { status = Just
                  { kind: Error
                  , text:
                      "Signing failed: " <> show err
                  }
              }

  CopyWitness -> do
    st <- H.get
    case st.witness of
      Nothing -> pure unit
      Just wit -> do
        liftEffect $ copyToClipboard wit
        H.modify_ _ { copyHint = true }
        H.liftAff $ delay (Milliseconds 2000.0)
        H.modify_ _ { copyHint = false }
