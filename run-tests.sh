#!/bin/sh
./bin/minerva-driver ./share/minerva/rts/ada-text_io.adb
./bin/minerva-driver ./share/minerva/rts/minerva-tests.adb
#exit
for i in `find ./share/minerva/tests -name "*.adb" -print`
do
   ./bin/minerva-driver $i
   status=$?
   echo -n $i ': '
   if [ $status -eq 0 ]; then
	   ../macro-11/bin/macro11-driver ./share/minerva/rts/pdp-11/artl.s $(basename $i .adb).s ada-text_io.s minerva-tests.s ./share/minerva/rts/pdp-11/basic_io.s
   	   if [ $status -eq 0 ]; then
		   mv a.out boot.o
		   cp boot.o $(basename $i .adb).o
	   	../pdp11/bin/pdp11-driver --quiet --execute
#	   	../pdp11/bin/pdp11-driver --quiet --execute --trace > $(basename $i .adb).trace
	   fi
   else
      echo failed
   fi
done

