module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> unit
    , render: \_ ->
        HH.div
          [ HP.class_ (HH.ClassName "container") ]
          [ HH.h1_ [ HH.text "Cardano Tx Signer" ]
          , HH.p
              [ HP.class_ (HH.ClassName "subtitle") ]
              [ HH.text
                  "Sign transactions with Eternl via CIP-30"
              ]
          ]
    , eval: H.mkEval H.defaultEval
    }
