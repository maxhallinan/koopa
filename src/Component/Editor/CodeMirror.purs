module Component.Editor.CodeMirror where

import Prelude

import Effect (Effect)
import Effect.Uncurried as U
import Web.HTML.HTMLElement (HTMLElement)

initCodeMirror :: HTMLElement -> String -> Effect CodeMirror
initCodeMirror = U.runEffectFn2 _initCodeMirror

foreign import data CodeMirror :: Type
foreign import _initCodeMirror :: U.EffectFn2 HTMLElement String CodeMirror
