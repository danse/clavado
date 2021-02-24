List the older files in the current directory and its
subdirectories. Tested on Mac.

### Install

 - clone the repo
 - activate your favourite Haskell virtual environment (optional)
 - from the repo root, install with `cabal install -j`

### Usage

    $ oldest <folder name>

### Example

    [hsenv] francesco$ oldest .
    Thu May 29 23:11:13 CEST 2014 "./Setup.hs"
    Thu May 29 23:21:30 CEST 2014 "./LICENSE"
    Thu May 29 23:27:01 CEST 2014 "./README.md"
    ...
