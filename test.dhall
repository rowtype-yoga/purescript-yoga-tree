let config = ./spago.dhall

in      config
    //  { sources = config.sources # [ "test/**/*.purs" ]
        , dependencies =
          [ "console"
          , "spec"
          , "aff"
          , "arrays"
          , "control"
          , "effect"
          , "foldable-traversable"
          , "free"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "tailrec"
          ]
        }
