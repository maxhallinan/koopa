{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "console"
    , "control"
    , "debug"
    , "effect"
    , "foldable-traversable"
    , "halogen"
    , "newtype"
    , "node-buffer"
    , "node-fs-aff"
    , "node-path"
    , "nonempty"
    , "ordered-collections"
    , "parsing"
    , "psci-support"
    , "quickcheck"
    , "spec"
    , "spec-quickcheck"
    , "strings"
    , "transformers"
    , "tuples"
    , "unicode"
    , "web-dom"
    , "web-html"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
