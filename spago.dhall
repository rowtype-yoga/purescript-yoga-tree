{ sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jordanmartinez/purescript-tree-rose"
, name = "tree-rose"
, dependencies =
  [ "arrays"
  , "control"
  , "foldable-traversable"
  , "free"
  , "maybe"
  , "prelude"
  , "tailrec"
  ]
, packages = ./packages.dhall
}
