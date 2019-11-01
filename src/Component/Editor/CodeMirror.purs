module Component.Editor.CodeMirror where

import Prelude

import Effect (Effect)
import Effect.Uncurried as U
import Web.HTML.HTMLElement (HTMLElement)

initCodeMirror :: HTMLElement -> String -> Effect CodeMirror
initCodeMirror = U.runEffectFn2 _initCodeMirror

onChange :: CodeMirror -> (String -> Effect Unit) -> Effect Unit
onChange = U.runEffectFn2 _onChange

foreign import data CodeMirror :: Type
foreign import _initCodeMirror :: U.EffectFn2 HTMLElement String CodeMirror
foreign import _onChange :: U.EffectFn2 CodeMirror (String -> Effect Unit) Unit
