#!/usr/bin/env fish
# Formats the haskell code with fourmolu
find src -name "*.hs" -exec fourmolu -m inplace "{}" ";"
find app -name "*.hs" -exec fourmolu -m inplace "{}" ";"
