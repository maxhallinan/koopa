module Component.Debugger where

import Prelude

import Component.Console as Console
import Component.Editor as Editor
import Component.Util (className)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type Input = { initialContent :: String }

type State = { initialContent :: String }

data Action

type ChildSlots = (editor :: Editor.Slot Unit, console :: Console.Slot Unit)

component :: forall q o m. MonadAff m => H.Component HH.HTML q Input o m
component = 
  H.mkComponent 
    { eval: H.mkEval H.defaultEval
    , initialState: identity
    , render
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div 
    [ className "debugger" ]
    [ HH.div 
        [ className "debugger-body grid" ] 
        [ HH.div [ className "column small-12" ] [ HH.slot (SProxy :: _ "editor") unit Editor.component state handleEditorMsg ]
        , HH.div [ className "column small-12" ] [ HH.slot (SProxy :: _ "console") unit Console.component unit handleConsoleMsg ]
        ]
    ]

handleEditorMsg :: Editor.Msg -> Maybe Action
handleEditorMsg msg = Nothing

handleConsoleMsg :: Console.Msg -> Maybe Action
handleConsoleMsg msg = Nothing
