<!-- omit in toc -->
# Getting started with Clash
This document will give a tour of all the files present in this starter project. It's also a general introduction into Clash dependency management. It's not an introduction to Clash itself though. If you're looking for an introduction to Clash, read ["Clash.Tutorial" on Hackage](https://hackage.haskell.org/package/clash-prelude).

```
{{name}}
├── bin
│   ├── Clash.hs
│   └── Clashi.hs
├── src
│   └── Example
│       └── Project.hs
├─── tests
│   ├── Tests
│   │   └── Example
│   │       └── Project.hs
│   └── tests.hs
├── cabal.project
├── {{name}}.cabal
└── stack.yaml
```

<!-- omit in toc -->
# Table of contents
- [Files](#files)
  - [{{name}}.cabal](#namecabal)
  - [cabal.project](#cabalproject)
  - [stack.yaml](#stackyaml)
  - [src/](#src)
  - [tests/](#tests)

# Files
## {{name}}.cabal
This is the most important file in your project. It describes how to build your project. Even though it ends in `.cabal`, Stack will use this file too. It starts of with meta information:

```yaml
cabal-version:       2.4
name:                {{name}}
version:             0.1
license:             BSD-2-Clause
author:              John Smith <john@example.com>
maintainer:          John Smith <john@example.com>
```

If you decide to publish your code on [Hackage](https://hackage.haskell.org/), this will show up on your package's front page. Take note of the license, it's set to `BSD-2-Clause` by default, but this might bee too liberal for your project. You can use any of the licenses on [spdx.org/licenses](https://spdx.org/licenses/). If none of those suit, remove the `license` line, add `license-file: LICENSE`, and add a `LICENSE` file of your choice to the root of this project. Moving on:

```yaml
common common-options
  default-extensions:
    BangPatterns
    BinaryLiterals
    ConstraintKinds
    [..]
    QuasiQuotes

    -- Prelude isn't imported by default as Clash offers Clash.Prelude
    NoImplicitPrelude
```

Clash's parent language is Haskell and its de-facto compiler, GHC, does a lot of heavy lifting before Clash gets to see anything. Because using Clash's Prelude requires a lot of extensions to be enabled to be used, we enable them here for all files in the project. Alternatively, you could add them where needed using `{-# LANGUAGE SomeExtension #-}` at the top of a `.hs` file instead. The next section, `ghc-options`, sets warning flags (`-Wall -Wcompat`) and flags that make GHC generate code Clash can handle.

Note that this whole section is a `common` "stanza". We'll use it as a template for any other definitions (more on those later). The last thing we add to the common section is some build dependencies:

```yaml
  build-depends:
    base,
    Cabal,

    -- clash-prelude will set suitable version bounds for the plugins
    clash-prelude >= 1.2.5 && < 1.4,
    ghc-typelits-natnormalise,
    ghc-typelits-extra,
    ghc-typelits-knownnat
```

These dependencies are fetched from [Hackage](https://hackage.haskell.org/), Haskell's store for packages. Next up is a `library` stanza. It defines where the source is located, in our case `src/`, and what modules can be found there. In our case that's just a single module, `Example.Project`.

```yaml
library
  import: common-options
  hs-source-dirs: src
  exposed-modules:
    Example.Project
  default-language: Haskell2010
```

Note that extra dependencies could be added by adding a `build-depends` line to this section. Last but not least, a testsuite stanza is defined:

```yaml
test-suite test-library
  import: common-options
  default-language: Haskell2010
  hs-source-dirs: tests
  type: exitcode-stdio-1.0
  ghc-options: -threaded
  main-is: tests.hs
  other-modules:
    Tests.Example.Project
  build-depends:
    {{name}},
    QuickCheck,
    hedgehog,
    tasty >= 1.2 && < 1.3,
    tasty-hedgehog,
    tasty-th
```

This test-suite, and if defined others, are executed when using `stack test` or `cabal test`. More on tests in [/tests](#tests).

## cabal.project
A `cabal.project` file is used to configure details of the build, more info can be found in the [Cabal user documentation](https://cabal.readthedocs.io/en/latest/cabal-project.html). We use it to disable a build flag on `clash-prelude`: `large-tuples`. It is ignored by Stack.

```yaml
packages:
  {{name}}.cabal

package clash-prelude
  -- 'large-tuples' generates tuple instances for various classes up to the
  -- GHC imposed maximum of 62 elements. This severely slows down compiling
  -- Clash, and triggers Template Haskell bugs on Windows. Hence, we disable
  -- it by default. This will be the default for Clash >=1.4.
  flags: -large-tuples
```

`cabal.project` can be used to build multi-package projects, by extending `packages`.

## stack.yaml
While Cabal fetches packages straight from Hackage (with a bias towards the latest versions), Stack works through _snapshots_. Snapshots are an index of packages from Hackage know to work well with each other. In addition to that, they specify a GHC version. These snapshots are curated by the community and FP Complete and can be found on [stackage.org](https://www.stackage.org/).

```yaml
resolver: lts-16.23

flags:
  clash-prelude:
    # 'large-tuples' generates tuple instances for various classes up to the
    # GHC imposed maximum of 62 elements. This severely slows down compiling
    # Clash, and triggers Template Haskell bugs on Windows. Hence, we disable
    # it by default. This will be the default for Clash >=1.4.
    large-tuples: false
```

This project uses [lts-16.23](https://www.stackage.org/lts-16.23), which includes Clash 1.2.5. If `{{name}}.cabal` constrains a dependency such that it cannot be fetched from the snapshot, Stack will ask you to add it to `stack.yaml`. Stack will then fetch it from Hackage. The point of this exercise is to make reproducible builds. Or in other words, if a `stack build` works now, it will work in 10 years too.

Similar to `cabal.project`, this is where we specify any build flags for dependencies.

## src/
This is where the source code of the project lives, as specified in `{{name}}.cabal`. It contains a single file, `Example/Project.hs`:

```haskell
module Example.Project (topEntity, plus) where

import Clash.Prelude

plus :: Signed 8 -> Signed 8 -> Signed 8
plus a b = a + b

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
topEntity :: Signed 8 -> Signed 8 -> Signed 8
topEntity = plus
```

as the comment says `topEntity` will get compiled by Clash if we ask it to compile this module:

```
stack run clash -- Example.Project --vhdl
```

or

```
cabal run clash --write-ghc-environment-files=always -- Example.Project --vhdl
```

We could instead ask it to synthesize `plus` instead:

```
stack run clash -- Example.Project --vhdl -main-is plus
```

If you want to play around with Clash, this is probably where you would put all the definitions mentioned in ["Clash.Tutorial" on Hackage](https://hackage.haskell.org/package/clash-prelude).

## tests/
Most of this directory is scaffolding, with the meat of the tests being defined in `tests/Tests/Example/Project.hs`. Writing good test cases is pretty hard: edge cases are easy to forget both in the implementation and tests. To this end, it's a good idea to use _fuzz testing_. In this project we use [Hedgehog](https://hedgehog.qa/):

```haskell
import Example.Project (plus)

prop_plusIsCommutative :: H.Property
prop_plusIsCommutative = H.property $ do
  a <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  b <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  plus a b === plus b a
```

This test generates two numbers `a` and `b` that fit neatly into domain of `Signed 8`, thanks to the use of `minBound` and `maxBound`. It then tests whether the `plus` operation commutes. I.e., whether `a + b ~ b + a`. All functions called `prop_*` are collected automatically:

```haskell
tests :: TestTree
tests = $(testGroupGenerator)
```

We can run the tests using `stack test` or `cabal run test-library --enable-tests`:

```
.
  Tests.Example.Project
    plusIsCommutative: OK
        ✓ plusIsCommutative passed 100 tests.

All 1 tests passed (0.00s)
```
