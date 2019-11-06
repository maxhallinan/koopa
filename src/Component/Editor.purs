module Component.Editor where

import Prelude

import Component.Editor.CodeMirror (CodeMirror)
import Component.Editor.CodeMirror as CodeMirror
import Component.Util (className)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Lang.Core (SrcLoc)

type Input = { initialContent :: String }

type State =
  { codeMirror :: Maybe CodeMirror
  , initialContent :: String
  }

data Action
  = Initialized
  | Finalized
  | EditorChanged String

data Query next
  = SetCursor SrcLoc next
  | SetEvalMode next
  | SetEditMode next

data Msg = ContentChanged String

type ChildSlots = ()

type Slot = H.Slot Query Msg

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Msg m
component =
  H.mkComponent
    { eval:
        H.mkEval
          $ H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , initialize = Just Initialized
            , finalize = Just Finalized
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
render _ =
  HH.div
    [ className "editor" ]
    [ HH.div
        [ HP.ref (H.RefLabel "codemirror") ]
        []
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Msg m Unit
handleAction = case _ of
  EditorChanged content ->
    H.raise $ ContentChanged content
  Initialized -> do
    H.getHTMLElementRef (H.RefLabel "codemirror") >>= traverse_ \element -> do
      { initialContent } <- H.get
      codeMirror <- H.liftEffect $ CodeMirror.initCodeMirror element initialContent
      void $ H.subscribe $ ES.effectEventSource \emitter -> do
        CodeMirror.onChange codeMirror (\content -> ES.emit emitter (EditorChanged content))
        pure mempty
      H.modify_ (_ { codeMirror = Just codeMirror })
  Finalized -> do
    H.modify_ (_ { codeMirror = Nothing })

handleQuery
  :: forall m a
   . MonadAff m
  => Query a
  -> H.HalogenM State Action ChildSlots Msg m (Maybe a)
handleQuery = case _ of
  SetCursor srcLoc next -> do
    { codeMirror } <- H.get
    case codeMirror of
      Just cm -> do
        H.liftEffect
          $ CodeMirror.setCursor
            { column: srcLoc.column
            , line: srcLoc.line - 1  -- CodeMirror line numbers are zero-based
            }
            cm
        pure (Just next)
      Nothing ->
        pure (Just next)
  SetEditMode next -> do
    { codeMirror } <- H.get
    case codeMirror of
      Just cm -> do
        H.liftEffect $ CodeMirror.styleActiveLine false cm
        pure (Just next)
      Nothing ->
        pure (Just next)
  SetEvalMode next -> do
    { codeMirror } <- H.get
    case codeMirror of
      Just cm -> do
        H.liftEffect $ CodeMirror.styleActiveLine true cm
        pure (Just next)
      Nothing ->
        pure (Just next)
