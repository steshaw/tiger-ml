#!/bin/bash

ml-build sources.cm Main.main && sml @SMLload=sources.x86-linux
