module Component.Editor where

import Prelude

import Component.Util (className)
import Component.Editor.CodeMirror (CodeMirror)
import Component.Editor.CodeMirror as CodeMirror
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input = { initialContent :: String }

type State =
  { codeMirror :: Maybe CodeMirror
  , initialContent :: String
  }

data Action = Initialize | Finalize

data Query a

data Msg

type ChildSlots = ()

type Slot = H.Slot Query Msg

component :: forall q m. MonadAff m => H.Component HH.HTML q Input Msg m
component =
  H.mkComponent
    { eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    , initialState
    , render
    }

initialState :: Input -> State
initialState i =
  { codeMirror: Nothing
  , initialContent: i.initialContent
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render _ = HH.div [ className "editor" ] [ HH.div [ HP.ref (H.RefLabel "codemirror") ] [] ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Msg m Unit
handleAction = case _ of
  Initialize -> do
    H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse_ \element -> do
      { initialContent } <- H.get
      codeMirror <- H.liftEffect $ CodeMirror.initCodeMirror element initialContent
      H.modify_ (_ { codeMirror = Just codeMirror })
  Finalize -> do
    H.modify_ (_ { codeMirror = Nothing })
