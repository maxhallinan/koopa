module Component.Debugger where

import Prelude

import Component.Console as Console
import Component.Editor as Editor
import Component.Util (className)
import Control.Monad.Trans.Class (lift)
import Coroutine (Yield(..), bounce)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (fst)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lang.Core (Bindings, ConsoleEffect, EvalT, LangEffect(..), PrimFns, runEvalT)
import Lang.Eval (eval)
import Lang.Parser (parseSequence)

type Input = { initialSourceCode :: String }

type State = 
  { consoleEffects :: Array ConsoleEffect
  , initialSourceCode :: String 
  , sourceCode :: String
  }

data Action 
  = RunClicked
  | SourceCodeChanged String 

type ChildSlots = 
  ( console :: Console.Slot Unit
  , editor :: Editor.Slot Unit
  )

component :: forall q o m. MonadAff m => H.Component HH.HTML q Input o m
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

runEval :: forall m. MonadAff m => (EvalT (Bindings (PrimFns m)) m) ~> m
runEval = map fst <<< runEvalT mempty

initialState :: Input -> State
initialState i = 
  { consoleEffects: []
  , initialSourceCode: i.initialSourceCode
  , sourceCode: i.initialSourceCode
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots (EvalT (Bindings (PrimFns m)) m)
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
            [ className "column small-7 editor" ] 
            [ HH.slot (SProxy :: _ "editor") unit (H.hoist H.lift Editor.component) editorInput handleEditorMsg ]
        , HH.div 
            [ className "column small-5 console" ] 
            [ HH.slot (SProxy :: _ "console") unit (H.hoist H.lift Console.component) consoleInput handleConsoleMsg ]
        ]
    , HH.div 
        [ className "toolbar" ] 
        [ HH.button 
          [ className "run-button" 
          , HE.onClick (const $ Just RunClicked)
          ] 
          [ HH.text "Run" ]
        ]
    ]

handleEditorMsg :: Editor.Msg -> Maybe Action
handleEditorMsg = case _ of 
  Editor.ContentChanged sourceCode ->
    Just (SourceCodeChanged sourceCode)

handleConsoleMsg :: Console.Msg -> Maybe Action
handleConsoleMsg msg = Nothing

handleAction :: forall m o. MonadAff m => Action ->  H.HalogenM State Action ChildSlots o (EvalT (Bindings (PrimFns m)) m) Unit
handleAction = case _ of
  RunClicked -> do
    { sourceCode } <- H.get
    interpret sourceCode
  SourceCodeChanged sourceCode ->
    H.modify_ (_ { sourceCode = sourceCode })

interpret :: forall m o. MonadAff m => String -> H.HalogenM State Action ChildSlots o (EvalT (Bindings (PrimFns m)) m) Unit
interpret sourceCode = do
  case parseSequence sourceCode of
    Left parseError -> do
      -- TODO: update state with parse error
      pure unit
    Right expr ->
      handleEffects (eval expr)
  where
  handleEffects = (\e -> go e) <=< lift <<< bounce
    where
    go output = do
      case output of
        Left (Yield (Breakpoint bindings) continue) -> do
          -- TODO: update state with bindings and continuation
          pure unit
        Left (Yield (Console consoleEffect) continue) -> do
          H.modify_ \s -> s { consoleEffects = A.cons consoleEffect s.consoleEffects }
          handleEffects (continue unit)
        Left (Yield (Throw evalError) _) -> do
          -- TODO: update state with eval error
          pure unit
        Right r ->
          -- discard result
          pure unit
