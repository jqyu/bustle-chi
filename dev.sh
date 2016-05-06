#!/bin/bash

stack build && stack exec chi-exe &

hobbes "*.hs" | while read _; do
  killall chi-exe
  stack build && stack exec chi-exe &
done
