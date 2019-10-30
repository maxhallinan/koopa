module Component.Util where

import Prelude

import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties (IProp, class_)

className :: forall i r. String -> IProp (class :: String | r) i
className  = class_ <<< ClassName
