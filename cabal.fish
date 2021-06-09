#!/usr/bin/env fish
cabal2nix . > wordcrab.nix
and nix-shell --run "cabal $argv"
