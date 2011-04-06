#!/bin/bash

image=sources.x86-linux

if [[ $(uname) = Darwin ]]; then
  image=sources.x86-darwin
fi

# Explicit call to ml-yacc needed here as there is a bug in CM (Compilation Manager).
ml-yacc tiger.grm &&
  ml-build sources.cm Main.main &&
  head -1 tiger.grm.desc &&
  sml @SMLload=${image} \
      ../testcases/*.tig \
      > run.actual.out &&
  diff run.expected.out run.actual.out &&
  echo "good!"

echo status=$?
