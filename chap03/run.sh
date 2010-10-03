#!/bin/bash

test20s=$(ls ../testcases/test2?.tig | grep -v ../testcases/test24.tig)
test30s=$(ls ../testcases/test3?.tig | grep -v ../testcases/test30.tig)
test40s=$(ls ../testcases/test4?.tig | grep -v ../testcases/test42.tig)

#test20s=$(ls ../testcases/test2?.tig)
#test30s=$(ls ../testcases/test3?.tig)
#test40s=$(ls ../testcases/test4?.tig)

# Explicit call to ml-yacc needed here as there is a bug in CM (Compilation Manager).
ml-yacc tiger.grm &&
  ml-build sources.cm Main.main &&
  head -1 tiger.grm.desc &&
  sml @SMLload=sources.x86-linux \
      *.tig \
      ../testcases/test?.tig \
      ../testcases/test1?.tig \
      ${test20s} \
      ${test30s} \
      ${test40s} \
      > run.actual.out &&
  diff run.expected.out run.actual.out &&
  echo "good!"

echo status=$?
