{ name = "typescript-utils"
, dependencies =
  [ "console"
  , "effect"
  , "functions"
  , "prelude"
  , "psci-support"
  , "typelevel-lists"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-typescript-utils.git"
}
