{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "lists"
  , "maybe"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "record"
  , "spec"
  , "spec-quickcheck"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
