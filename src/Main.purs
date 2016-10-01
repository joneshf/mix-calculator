module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.MonadZero (guard)

import Data.Int (fromString, round, toNumber)
import Data.Traversable (for)

import Halogen (HalogenEffects, ComponentDSL, ComponentHTML, Component, runUI, component, modify)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Util (runHalogenAff, awaitBody)

type State
  = { bond :: Int
    , mix :: Int
    , purchase :: Int
    , stock :: Int
    , newStock :: Int
    , newBond :: Int
    }

data Query a
  = UpdateStock String a
  | UpdateBond String a
  | UpdatePurchase String a
  | UpdateMix String a

initialState :: State
initialState =
  { bond: 0
  , stock: 0
  , mix: 50
  , purchase: 0
  , newStock: 0
  , newBond: 0
  }

ui :: forall g. Component State Query g
ui = component { render, eval }
  where
  render :: State -> ComponentHTML Query
  render state =
    H.section_
      [ H.h1_
        [ H.text "Mix Calculator"
        ]
      , H.section_
        [ H.h2_
          [ H.text "Current"
          ]
        , H.div_
          [ H.label
            [ P.for "stock" ]
            [ H.text "Stock"]
          , H.input
            [ P.id_ "stock"
            , P.placeholder "stock"
            , E.onValueChange (E.input UpdateStock)
            ]
          ]
        , H.div_
          [ H.label
            [ P.for "bond" ]
            [ H.text "Bond"]
          , H.input
            [ P.id_ "bond"
            , P.placeholder "bond"
            , E.onValueChange (E.input UpdateBond)
            ]
          ]
        , H.div_
          [ H.label
            [ P.for "purchase" ]
            [ H.text "Purchase"]
          , H.input
            [ P.id_ "purchase"
            , P.placeholder "purchase"
            , E.onValueChange (E.input UpdatePurchase)
            ]
          ]
        , H.div_
          [ H.label
            [ P.for "mix" ]
            [ H.text "Mix"]
          , H.input
            [ P.id_ "mix"
            , P.inputType P.InputRange
            , E.onValueChange (E.input UpdateMix)
            ]
          , H.span_
            [ H.text $ show state.mix <> "% stock"
            ]
          , H.span_
            [ H.text $ show (100 - state.mix) <> "% bond"
            ]
          ]
        ]
      , H.section_
        [ H.h2_
          [ H.text "Amount to Purchase"
          ]
        , H.div_
          [ H.span_
            [ H.text $ "Stock: " <> show state.newStock
            ]
          , H.span_
            [ H.text $ "Bond: " <> show state.newBond
            ]
          ]
        ]
      ]

  eval :: Query ~> ComponentDSL State Query g
  eval (UpdateStock str next) = do
    let mstock = fromString str
    for mstock \stock ->
      modify (_ {stock = stock})
    modify recalculate
    pure next
  eval (UpdateBond str next) = do
    let mbond = fromString str
    for mbond \bond ->
      modify (_ {bond = bond})
    modify recalculate
    pure next
  eval (UpdatePurchase str next) = do
    let mpurchase = fromString str
    for mpurchase \purchase ->
      modify (_ {purchase = purchase})
    modify recalculate
    pure next
  eval (UpdateMix str next) = do
    let mmix = do n <- fromString str
                  guard $ 0 <= n
                  guard $ n <= 100
                  pure n
    for mmix \mix ->
      modify (_ {mix = mix})
    modify recalculate
    pure next

recalculate :: State -> State
recalculate state =
  state
    { newStock = clamp 0 state.purchase newStock
    , newBond = clamp 0 state.purchase $ state.purchase - newStock
    }
  where
  total = state.stock + state.bond + state.purchase
  newStock = round $ toNumber total * (toNumber state.mix / 100.0) - toNumber state.stock

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
