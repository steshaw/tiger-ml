#!/bin/bash

image=sources.x86-linux

if [[ $(uname) = Darwin ]]; then
  image=sources.x86-darwin
fi

ml-build sources.cm Main.main && 
  sml @SMLload=${image} > run.actual.out &&
  diff run.expected.out run.actual.out &&
  echo "good!"
