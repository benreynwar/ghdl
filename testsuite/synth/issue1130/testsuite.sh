#! /bin/sh

. ../../testenv.sh

for t in foo; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

echo "Test successful"
