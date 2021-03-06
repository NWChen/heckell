#!/bin/bash

HECKELL="./heckell"

# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog

Compare() {
    if diff $1 $2 &> /dev/null ; then
        echo "SUCCESS"
    else
        echo "FAIL"
        cat $1
        cat $2
    fi
}

files="tests/test-*.hck tests/fail-*.hck"

for file in $files
do
    basename=`echo $file | sed 's/.*\\///
                                s/.hck//'`
    echo -n "$basename..."



    case $file in
    *test-*)
        $HECKELL $file > "tests/${basename}.ll"
        $LLC "tests/${basename}.ll" > "tests/${basename}.s"
        $CC -o "tests/${basename}.exe" "tests/${basename}.s" "ut-sets.o"
        "./tests/${basename}.exe" > "tests/${basename}.diff"
        Compare tests/${basename}.diff tests/${basename}.out
        ;;
    *fail-*)
        $HECKELL $file 2> "tests/${basename}.diff"
        Compare tests/${basename}.diff tests/${basename}.err
        ;;
    *)      
        echo "unknown file type $file"
    esac

    # Remove generated file
    rm -f tests/${basename}.diff tests/${basename}.ll tests/${basename}.exe tests/${basename}.s

done
