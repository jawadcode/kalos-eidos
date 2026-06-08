#!/usr/bin/env bash

llvm-as <"$1" | opt -passes=dot-cfg -disable-output
dot -Tsvg ."$2".dot -o "$2".svg
xdg-open "$2".svg
