#!/bin/bash

# Explicit call to ml-yacc needed here as there is a bug in CM (Compilation Manager).
ml-yacc tiger.grm &&
  ml-build sources.cm Main.main &&
  sml @SMLload=sources.x86-linux > run.actual.out &&
  diff run.expected.out run.actual.out &&
  echo "good!"

echo status=$?
