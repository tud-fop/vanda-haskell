# Vanda-Haskell
Vanda-Haskell is a toolkit that contains various proof-of-concept implementations of algorithms from natural language processing.

## Install Vanda-Haskell using cabal
* you need to have `ghc` and `cabal` installed
* change to the project’s root directory
* (optionally) create a sandbox via `cabal sandbox init`
* install via `cabal install --enable-documentation`

The installation provides the command line tool `vanda` and a Haskell library.

## Usage of the command line tool
* You can run the tool via `cabal exec -- vanda` from the project’s root directory.
  Alternatively you can locate the binary via `cabal exec which vanda` and run it directly or add the containing directory to your `PATH`.
* Algorithms are called via git-style subcommands, e.g.,
  `cabal exec -- vanda brackets mdyck '[(,{' ']),}' '{([])[()]}'`.
* Each subcommand has its own `help` subcommand that provides further information, e.g., `cabal exec -- vanda brackets help`.
* Running `vanda` without arguments shows you the following list of available top-level subcommands:

    ```
    vanda [COMMAND] ...
      Vanda

    Commands:
      brackets   algorithms for Dyck and multiple Dyck languages
      cbsm       Count-Based State Merging
      lcfrs      algorithms for probabilistic LCFRS
      pmcfg      algorithms for weighted parallel multiple context-free grammars
      ngrams     algorithms for n-gram models
      pdta       grammatical inference of probabilistic deterministic tree
                 automata (experimental!)
      xrsngrams  algorithms for combining n-gram models and translation models
      pcfg       algorithms for extracting and training PCFGs

    Every mode provides a sub-mode help, which displays only help concerning this
    mode. Every help mode has an optional argument which defines the help format.
    ```

## Install and Usage with stack
* You need to have `stack` installed.
* Change to the project’s root directory and
* install via `stack build --copy-bins`.

* You can run the tool via `stack exec -- vanda` from any directory.
  Alternatively you can locate the binary via `stack exec which vanda` and run it directly or add the containing directory to your `PATH`.

## The Vanda Haskell library
Currently, the library just exposes nearly every module there is in the project.
This includes the modules that implement the functionality of the command line tool.
The API is not fixed, yet.

The modules are documented using haddock syntax.
While installation `cabal` collects the haddock documentation and API information to create easy-to-read HTML pages if the `--enable- documentation` flag is used or `documentation: True` is set in cabal’s configuration file.
The location of the documentation is output during the installation.
