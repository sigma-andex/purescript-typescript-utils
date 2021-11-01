{ name = "typescript-utils"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-typescript-utils.git"
}
