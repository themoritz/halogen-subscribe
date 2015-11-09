module Example.Intro where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

data Query a
  = ToggleState a
  | Charge a

ui :: Component State Query Ex
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button" ]
      , H.button
          [ E.onClick (E.input_ Charge) ]
          [ H.text "Charge" ]
      , H.button
          [ P.id_ "fire" ]
          [ H.text "Fire" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "On" else "Off") ]
      ]

  eval :: Natural Query (ComponentDSL State Query Ex)
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next

  eval (Charge next) = do
    subscribe $ eventSource_ (attachHandler "#fire") do
      log "moo"
      pure $ action ToggleState
    pure next

type Effects = HalogenEffects (console :: CONSOLE)
type Ex = Aff Effects

main :: Eff Effects Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node

foreign import attachHandler :: forall eff. String -> Eff eff Unit -> Eff eff Unit
