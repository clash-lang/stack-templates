<!-- omit in toc -->
# Simple Starter Project
This starter project contains the scaffolding needed to integrate Clash with the Cabal and Stack build systems. It allows you to use dependencies from [Hackage](https://hackage.haskell.org/) easily.

<!-- omit in toc -->
# Table of Contents
- [Getting this project](#getting-this-project)
- [Building and testing this project](#building-and-testing-this-project)
  - [Stack (Windows, Linux, MacOS) [recommended]](#stack-windows-linux-macos-recommended)
  - [Cabal (Linux, MacOS)](#cabal-linux-macos)
- [REPL](#repl)
- [Extending this project](#extending-this-project)
- [Change the license](#change-the-license)
- [IDE support](#ide-support)

# Getting this project
Stack users can run `stack new my-clash-project clash-lang/simple`. Cabal users can [download a zip](https://raw.githubusercontent.com/clash-lang/clash-starters/main/simple.zip) containing the project.

# Building and testing this project
There's a number of ways to build this project on your machine. The recommended way of doing so is using _Stack_, whose instructions will follow directly after this section.

## Stack (Windows, Linux, MacOS) [recommended]
Install Stack using your package manager or refer to the [How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install) section of the [Stack manual](https://docs.haskellstack.org/en/stable/README/).

Build the project with:

```bash
stack build
```

To run the tests defined in `tests/`, use:

```bash
stack test
```

To compile the project to VHDL, run:

```bash
stack run clash -- Example.Project --vhdl
```

You can find the HDL files in `vhdl/`. The source can be found in `src/Example/Project.hs`.

## Cabal (Linux, MacOS)
**The following instructions only work for Cabal >=3.0 and GHC >=8.4.**

First, update your cabal package database:

```bash
cabal update
```

You only have to run the update command once. After that, you can keep rebuilding your project by running the build command:

```bash
cabal build
```

To run the tests defined in `tests/`, use:

```bash
cabal run test-library --enable-tests
```

To compile the project to VHDL, run:

```bash
cabal run clash --write-ghc-environment-files=always -- Example.Project --vhdl
```

You can find the HDL files in `vhdl/`. The source can be found in `src/Example/Project.hs`.

# REPL
Clash offers a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) as a quick way to try things, similar to Python's `python` or Ruby's `irb`. Stack users can open the REPL by invoking:

```
stack run clashi
```

Cabal users use:

```
cabal run --write-ghc-environment-files=always clashi
```

# Extending this project
Head over to [TOUR.md](TOUR.md).

# Change the license
By default `{{name}}.cabal` sets its `license` field to `BSD-2-Clause`. You might want to change this.

# IDE support
We currently recommend Visual Studio Code in combination with the _Haskell_ plugin. All you need to do is open this folder in VSCode; it will prompt you to install the plugin.
