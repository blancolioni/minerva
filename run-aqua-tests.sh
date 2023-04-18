#!/bin/sh
for i in `find config/tests -name "*.adb" -print`
do
   echo -n $i ': '
   ./aqua-run.sh $i
done

