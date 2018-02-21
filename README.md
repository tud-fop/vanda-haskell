# Vanda-Haskell
Vanda-Haskell is a toolkit that contains various proof-of-concept implementations of algorithms from natural language processing.

## Install Vanda-Haskell
* you need to have `ghc` and `cabal` installed
* install with 'cabal install'

## Usage
* Vanda-Haskell is called from the command line.
* Ensure that the binary of Vanda-Haskell (named `Vanda`) is within you `PATH`.
* Algorithms are called via git-style subcommands.
* Each subcommand has its own `help` subcommand that provides further information.
* top-level commands:
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
