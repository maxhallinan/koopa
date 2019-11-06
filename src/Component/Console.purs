module Component.Console where

import Prelude

import Component.Util (className)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lang.Core (ConsoleEffect(..), ExprAnn, SrcLoc)

type Input =
  { consoleEffects :: Array ConsoleEffect
  }

type State =
  { activeSection :: Section
  , bindings :: Maybe (Map String ExprAnn)
  , consoleEffects :: Array ConsoleEffect
  }

data Section
  = Output
  | Variables

derive instance eqSection :: Eq Section

data Action
  = InputReceived Input
  | TabItemClicked Section

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
  { activeSection: Output
  , bindings: Nothing
  , consoleEffects: i.consoleEffects
  }

handleAction :: forall m. MonadAff m => Action ->  H.HalogenM State Action ChildSlots Msg m Unit
handleAction = case _ of
  InputReceived input ->
    H.modify_ (_ { consoleEffects = input.consoleEffects })
  TabItemClicked section ->
    H.modify_ (_ { activeSection = section })

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div
    [ className "console-section" ]
    [ renderTabNav state
    , renderActiveSection state
    ]

renderTabNav :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderTabNav state =
  let
    items =
      [ { isDisabled: false
        , label: "Output"
        , section: Output
        }
      , { isDisabled: state.bindings == Nothing
        , label: "Variables"
        , section: Variables
        }
      ]
  in
  HH.header
    [ className "tab-nav" ]
    $ map (renderTabItem state) items

type TabItem =
  { isDisabled :: Boolean
  , label :: String
  , section :: Section
  }

renderTabItem :: forall m. MonadAff m => State -> TabItem -> H.ComponentHTML Action ChildSlots m
renderTabItem { activeSection } tabItem =
  let
    cn = if activeSection == tabItem.section
           then "console-title tab-item active"
           else "console-title tab-item"
  in
  HH.h2
    [ className cn ]
    [ HH.button
        [ HP.disabled tabItem.isDisabled
        , HE.onClick (const $ Just $ TabItemClicked tabItem.section)
        ]
        [ HH.text tabItem.label ]
    ]

renderActiveSection :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderActiveSection state =
  case state.activeSection of
    Output ->
      renderOutput state
    Variables ->
      renderVariables state

renderOutput :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderOutput state =
  HH.div
    [ className "console-output" ]
    $ map renderConsoleEffects state.consoleEffects

renderConsoleEffects :: forall m. MonadAff m => ConsoleEffect -> H.ComponentHTML Action ChildSlots m
renderConsoleEffects = case _ of
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

renderVariables :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderVariables state =
  HH.text "variables"
