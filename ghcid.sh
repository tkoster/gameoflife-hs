#!/bin/bash

# Install trap to restore cursor after Ctrl-C.
cnorm() {
  tput cnorm
}
trap cnorm EXIT INT

stack exec ghcid -- --command "stack ghci gameoflife:exe:gameoflife"
