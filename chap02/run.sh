#!/bin/bash

ml-build sources.cm Main.main && 
  sml @SMLload=sources.x86-linux > run.actual.out &&
  diff run.expected.out run.actual.out &&
  echo "good!"
