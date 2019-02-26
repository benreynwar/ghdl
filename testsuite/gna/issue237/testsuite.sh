#! /bin/sh

. ../../testenv.sh

analyze with_record_port.vhd
elab with_record_port

if ghdl_has_feature with_record_port vpi; then
  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  if [ "$OS" = "Windows_NT" ]; then
      vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\\\!/!g' -e 's!^C:!/C!g'`
      echo vpi_lib: $vpi_lib
      PATH="$PATH:$vpi_lib"
  fi

  simulate with_record_port --vpi=./vpi1.vpi | tee with_record_port.out
  if grep -q Error with_record_port.out; then
      echo "Error in output"
      rm -f vpi1.vpi vpi1.o with_record_port.out
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o with_record_port.out
fi
clean

echo "Test successful"
