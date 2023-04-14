#!/bin/sh
./build/bin/minerva-driver $1
if [ $? -eq 0 ]; then
  ../aquarius/tools/aqua/as/build/bin/as-driver ../aquarius/tools/aqua/as/config/artl.s m__$(basename $1 .adb).s $(basename $1 .adb).s ./config/arch/aqua/aqua.s
  if [ $? -eq 0 ]; then
    ../aquav3/build/bin/aqua-driver artl.aqo
  fi
fi
