module Component.Editor.CodeMirror where

import Prelude

import Effect (Effect)
import Effect.Uncurried as U
import Web.HTML.HTMLElement (HTMLElement)

initCodeMirror :: HTMLElement -> String -> Effect CodeMirror
initCodeMirror = U.runEffectFn2 _initCodeMirror

onChange :: CodeMirror -> (String -> Effect Unit) -> Effect Unit
onChange = U.runEffectFn2 _onChange

setCursor :: { column :: Int, line :: Int } -> CodeMirror -> Effect Unit
setCursor = U.runEffectFn2 _setCursor

setReadOnly :: Boolean -> CodeMirror -> Effect Unit
setReadOnly = U.runEffectFn2 _setReadOnly

styleActiveLine :: Boolean -> CodeMirror -> Effect Unit
styleActiveLine = U.runEffectFn2 _styleActiveLine

foreign import data CodeMirror :: Type
foreign import _initCodeMirror :: U.EffectFn2 HTMLElement String CodeMirror
foreign import _onChange :: U.EffectFn2 CodeMirror (String -> Effect Unit) Unit
foreign import _setCursor :: U.EffectFn2 { column :: Int, line :: Int } CodeMirror Unit
foreign import _setReadOnly :: U.EffectFn2 Boolean CodeMirror Unit
foreign import _styleActiveLine :: U.EffectFn2 Boolean CodeMirror Unit
