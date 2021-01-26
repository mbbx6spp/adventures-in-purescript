{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arrays"
  , "checked-exceptions"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "generics-rep"
  , "integers"
  , "newtype"
  , "nonbili-postgres"
  , "prelude"
  , "psci-support"
  , "selective"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
