module Component.Debugger where

import Prelude

import Component.Console as Console
import Component.Editor as Editor
import Component.Util (className)
import Control.Monad.State.Trans (withStateT)
import Control.Monad.Trans.Class (lift)
import Coroutine (Yield(..), bounce)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (fst)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lang.Core (Bindings, ConsoleEffect, Eval, EvalState, EvalT(..), ExprAnn, LangEffect(..), PrimFns, SrcSpan, runEvalT)
import Lang.Eval (eval)
import Lang.Parser (parseSequence)

type Input = { initialSourceCode :: String }

type State m =
  { consoleEffects :: Array ConsoleEffect
  , initialSourceCode :: String
  , interpreterState :: InterpreterState m
  , sourceCode :: String
  }

data InterpreterState m
  = Ready
  | Suspended (BreakpointState m)
  | Running

type BreakpointState m =
  { continue :: Unit -> Eval m ExprAnn
  , evalState :: EvalState (Bindings (PrimFns m))
  , srcSpan :: SrcSpan
  }

data Action
  = RunClicked
  | ContinueClicked
  | SourceCodeChanged String

type ChildSlots =
  ( console :: Console.Slot Unit
  , editor :: Editor.Slot Unit
  )

component
  :: forall q o m
   . MonadAff m
  => H.Component HH.HTML q Input o m
component = H.hoist runEval
  $ H.mkComponent
      { eval:
          H.mkEval
            $ H.defaultEval
                { handleAction = handleAction
                }
      , initialState
      , render
      }

runEval
  :: forall m
   . MonadAff m
  => (EvalT (Bindings (PrimFns m)) m)
  ~> m
runEval = map fst <<< runEvalT mempty

initialState :: forall m. MonadAff m => Input -> State m
initialState i =
  { consoleEffects: []
  , interpreterState: Ready
  , initialSourceCode: i.initialSourceCode
  , sourceCode: i.initialSourceCode
  }

render
  :: forall m
   . MonadAff m
  => State m
  -> H.ComponentHTML Action ChildSlots (EvalT (Bindings (PrimFns m)) m)
render state =
  let
    editorInput = { initialContent: state.sourceCode }

    consoleInput = { consoleEffects: state.consoleEffects }
  in
  HH.div
    [ className "debugger" ]
    [ HH.div
        [ className "debugger-body grid" ]
        [ HH.div
            [ className "column small-7" ]
            [ HH.slot (SProxy :: _ "editor") unit (H.hoist H.lift Editor.component) editorInput handleEditorMsg ]
        , HH.div
            [ className "column small-5" ]
            [ HH.slot (SProxy :: _ "console") unit (H.hoist H.lift Console.component) consoleInput handleConsoleMsg ]
        ]
    , HH.div
        [ className "toolbar" ]
        [ renderEvalButton state
        ]
    ]

renderEvalButton
  :: forall m
   . MonadAff m
  => State m
  -> H.ComponentHTML Action ChildSlots (EvalT (Bindings (PrimFns m)) m)
renderEvalButton state =
  case state.interpreterState of
    Ready ->
      HH.button
        [ className "run-button"
        , HE.onClick (const $ Just RunClicked)
        ]
        [ HH.text "Run" ]
    Suspended _ ->
      HH.button
        [ className "run-button"
        , HE.onClick (const $ Just ContinueClicked)
        ]
        [ HH.text "Continue" ]
    Running ->
      HH.button
        [ className "run-button" ]
        [ HH.text "..." ]

handleEditorMsg :: Editor.Msg -> Maybe Action
handleEditorMsg = case _ of
  Editor.ContentChanged sourceCode ->
    Just (SourceCodeChanged sourceCode)

handleConsoleMsg :: Console.Msg -> Maybe Action
handleConsoleMsg msg = Nothing

handleAction
  :: forall m o
   . MonadAff m
  => Action
  -> H.HalogenM (State m) Action ChildSlots o (EvalT (Bindings (PrimFns m)) m) Unit
handleAction = case _ of
  RunClicked -> do
    { sourceCode } <- H.modify (_ { interpreterState = Running })
    interpret sourceCode
  ContinueClicked -> do
    { interpreterState } <- H.get
    case interpreterState of
      Suspended { continue, evalState } -> do
        H.modify_ (_ { interpreterState = Running })
        let EvalT stateT = bounce (continue unit)
        langEffect <- lift $ EvalT $ withStateT (const evalState) stateT
        handleLangEffect langEffect
      _ ->
        pure unit
  SourceCodeChanged sourceCode ->
    H.modify_ (_ { sourceCode = sourceCode })

interpret
  :: forall m o
   . MonadAff m
  => String
  -> H.HalogenM (State m) Action ChildSlots o (EvalT (Bindings (PrimFns m)) m) Unit
interpret sourceCode = do
  case parseSequence sourceCode of
    Left parseError -> do
      H.modify_ (_ { interpreterState = Ready })
    Right expr -> do
      void $ H.queryAll (SProxy :: SProxy "editor") (H.tell $ Editor.SetEvalMode)
      langEffect <- lift $ bounce (eval expr)
      handleLangEffect langEffect

handleLangEffect
  :: forall m o
   . MonadAff m
  => Either (Yield Unit (LangEffect m) (Eval m ExprAnn)) ExprAnn
  -> H.HalogenM (State m) Action ChildSlots o (EvalT (Bindings (PrimFns m)) m) Unit
handleLangEffect output = do
  case output of
    Left (Yield (Breakpoint evalState ann) continue) -> do
      let breakpointState = { continue, evalState, srcSpan: ann.srcSpan }
      H.modify_ \s -> s { interpreterState = Suspended breakpointState }
      void $ H.queryAll (SProxy :: SProxy "editor") (H.tell $ Editor.SetCursor ann.srcSpan.begin)
      let env = _.env (unwrap (_.bindings $ unwrap (evalState)))
      void $ H.queryAll (SProxy :: SProxy "console") (H.tell $ Console.DisplayBindings env)
    Left (Yield (Console consoleEffect) continue) -> do
      H.modify_ \s -> s { consoleEffects = A.cons consoleEffect s.consoleEffects }
      langEffect <- lift $ bounce (continue unit)
      handleLangEffect langEffect
    Left (Yield (Throw evalError) _) -> do
      -- TODO: update state with eval error
      void $ H.queryAll (SProxy :: SProxy "editor") (H.tell $ Editor.SetEditMode)
      H.modify_ (_ { interpreterState = Ready })
    Right r -> do
      void $ H.queryAll (SProxy :: SProxy "editor") (H.tell $ Editor.SetEditMode)
      H.modify_ (_ { interpreterState = Ready })
