module Component.Console where

import Prelude

import Component.Util (className)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Lang.Core (ConsoleEffect(..), ExprAnn, SrcLoc)

type Input = 
  { consoleEffects  :: Array ConsoleEffect
  }

type State =
  { consoleEffects :: Array ConsoleEffect
  }

data Action = InputReceived Input

data Query a

data Msg

type ChildSlots = ()

type Slot = H.Slot Query Msg

component :: forall q m. MonadAff m => H.Component HH.HTML q Input Msg m
component = 
  H.mkComponent 
    { eval: 
      H.mkEval 
        $ H.defaultEval 
          { handleAction = handleAction 
          , receive = Just <<< InputReceived
          }
    , initialState
    , render
    }

initialState :: Input -> State
initialState i =
  { consoleEffects: i.consoleEffects
  }

handleAction :: forall m. MonadAff m => Action ->  H.HalogenM State Action ChildSlots Msg m Unit
handleAction = case _ of
  InputReceived input ->
    H.modify_ (_ { consoleEffects = input.consoleEffects })

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div 
    [ className "console-section" ] 
    [ HH.h2 
        [ className "console-title" ] 
        [ HH.text "Output" ]
    , HH.div 
        [ className "console-output" ] 
        $ map renderOutput state.consoleEffects
    ]

renderOutput :: forall m. MonadAff m => ConsoleEffect -> H.ComponentHTML Action ChildSlots m
renderOutput = case _ of
  ConsoleLog exprs srcSpan ->
    HH.p 
      [ className "grid console-log" ] 
      [ renderLog exprs
      , renderSrcLoc srcSpan.begin
      ]

renderLog :: forall m. MonadAff m => List ExprAnn -> H.ComponentHTML Action ChildSlots m
renderLog exprs = 
  HH.span 
    [ className "column small-10" ] 
    [ HH.text (intercalate " " $ map show exprs) ]

renderSrcLoc :: forall m. MonadAff m => SrcLoc -> H.ComponentHTML Action ChildSlots m
renderSrcLoc { column, line } =
  HH.span 
    [ className "column small-2 console-srcloc" ] 
    [ HH.text (show line <> ":" <> show column) ]
