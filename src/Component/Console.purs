module Component.Console where

import Prelude

import Component.Util (className)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = Unit

data Action

data Query a

data Msg

type ChildSlots = ()

type Slot = H.Slot Query Msg

component :: forall q i m. MonadAff m => H.Component HH.HTML q i Msg m
component = 
  H.mkComponent 
    { eval: H.mkEval H.defaultEval
    , initialState: const unit
    , render
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div 
    [ className "console" ] 
    []
