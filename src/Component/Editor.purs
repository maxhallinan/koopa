module Component.Editor where

import Prelude

import Component.Util (className)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as P

type Input = { initialContent :: String }

type State = { initialContent :: String }

data Action

data Query a

data Msg

type ChildSlots = ()

type Slot = H.Slot Query Msg

component :: forall q o m. Monad m => H.Component HH.HTML q Input Msg m
component = 
  H.mkComponent 
    { eval: H.mkEval H.defaultEval
    , initialState
    , render
    }

initialState :: Input -> State
initialState = identity

render :: forall m. Monad m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div 
    [ className "editor" ] 
    [ HH.textarea 
        [ P.rows 15 
        , P.value state.initialContent
        ] 
    ]
