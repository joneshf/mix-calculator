module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Int (round)
import DOM (DOM)
import Flare (UI, runFlare, number, numberSlider, fieldset)
import Signal.Channel (CHANNEL)
import Test.FlareCheck (class Interactive, class Flammable, Renderable(SetText), flareCheck', spark)

foreign import currency :: Number -> String

newtype Stock = Stock Number
newtype Bond = Bond Number
newtype Mix = Mix Number
newtype Additional = Additional Number
data Buy = Buy Stock Bond Mix

instance flammableStock :: Flammable Stock where
  spark = Stock <$> number "Stock" 6000.0

instance flammableBond :: Flammable Bond where
  spark = Bond <$> number "Bond" 4000.0

instance flammableMix :: Flammable Mix where
  spark = Mix <$> numberSlider "Mix" 0.0 1.0 0.01 0.6

instance flammableAdditional :: Flammable Additional where
  spark = Additional <$> number "Additional" 500.0

instance flammableBuy :: Flammable Buy where
  spark = recalculate <$> spark <*> spark <*> spark <*> spark

instance interactiveBuy :: Interactive Buy where
  interactive = map (SetText <<< pretty)

pretty :: Buy -> String
pretty (Buy stock bond mix) = pretty' stock bond mix

pretty' :: Stock -> Bond -> Mix -> String
pretty' (Stock stock) (Bond bond) (Mix mix) =
  "To reach a mix of "
    <> show (round $ mix * 100.0)
    <> "% stocks and "
    <> show (round $ (1.0 - mix) * 100.0)
    <> "% bonds, you should purchase "
    <> currency stock
    <> " in stock and "
    <> currency bond
    <> " in bonds."

recalculate :: Stock -> Bond -> Mix -> Additional -> Buy
recalculate stock bond mix (a@Additional additional) =
  Buy (Stock newStock') (Bond $ additional - newStock') mix
  where
  newStock' = newStock stock bond mix a

newStock :: Stock -> Bond -> Mix -> Additional -> Number
newStock (Stock stock) (Bond bond) (Mix mix) (Additional additional) =
  clamp 0.0 additional $ (stock + bond + additional) * mix - stock

ui :: forall a. UI a String
ui = fieldset "Mix Calculator" (pretty <$> spark)

main :: Eff (dom :: DOM, channel :: CHANNEL) Unit
main = do
  runFlare "input" "output" ui
  checks

checks :: Eff (dom :: DOM, channel :: CHANNEL) Unit
checks = do
  flareCheck' "checks" "pretty" pretty
  flareCheck' "checks" "pretty'" pretty'
  flareCheck' "checks" "recalculate" recalculate
  flareCheck' "checks" "newStock" newStock
