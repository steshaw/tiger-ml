#!/bin/bash

# Explicit call to ml-yacc needed here as there is a bug in CM (Compilation Manager).
ml-yacc tiger.grm &&
  ml-build sources.cm Main.main &&
  head -1 tiger.grm.desc &&
  (
    # FIXME: Hack to make the sort-order the same on Mac/Darwin as on Linux.
    # FIXME: Luckily we don't use 'z' in our testcase file names...
    files=$(ls ../testcases/*.tig | sed 's/-/z/g' | sort | sed -e 's/z/-/g')
    for tigerSource in $files; do
      echo -- $tigerSource --
      cat $tigerSource
      echo --
      ./tigerc $tigerSource
      echo
    done
  ) > run.actual.out &&
  diff -U5 run.expected.out run.actual.out | tee run.diff.out && echo "good!"
echo status=$?
