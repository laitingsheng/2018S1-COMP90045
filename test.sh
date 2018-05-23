make
mkdir -p output
rm -f output/*.err output/*.diff

for k in $(find . -name "*.out")
do
    j=${k%.out}
    i=$(basename -- "$j")
    echo "current paz $i"
    ./Paz $j.paz > output/$i.taz 2> output/output-$i.err
    if [ ! -s output/output-$i.err ]
    then
        echo "    no warnings"
    else
        echo "    fail to compile"
        echo
        continue
    fi
    if [ -f $j.in ]
    then
        taz_emulator/taz output/$i.taz < $j.in > output/output-$i
    else
        taz_emulator/taz output/$i.taz > output/output-$i
    fi
    diff output/output-$i $k > output/output-$i.diff
    if [ ! -s output/output-$i.diff ]
    then
        echo "    matched"
    else
        echo "    unmatched"
    fi
    echo
done
