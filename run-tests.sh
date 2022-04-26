#!/bin/sh
for i in `find config/tests -name "*.adb" -print`
do
   ./build/bin/minerva-driver $i --target=pdp-11 --compile-only
   status=$?
   echo -n $i ': '
   if [ $status -eq 0 ]; then
      pdp11 --source=$(basename $i .adb).s
   else
      echo failed
   fi
done

