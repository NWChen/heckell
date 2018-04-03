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

Compare() {
    if diff $1 $2 &> /dev/null ; then
		echo "SUCCESS"
	else
		echo "FAIL"
	fi
}

files="tests/test-*.hck"

for file in $files
do
    basename=`echo $file | sed 's/.*\\///
                         		s/.hck//'`
	echo -n "$basename..."

    $HECKELL $file > "tests/${basename}.ll"
    $LLC "tests/${basename}.ll" > "tests/${basename}.s"
    $CC -o "tests/${basename}.exe" "tests/${basename}.s"
    "./tests/${basename}.exe" > "tests/${basename}.diff"

    case $file in
	*test-*)
	    Compare tests/${basename}.diff tests/${basename}.out
	    ;;
	*)
	    echo "unknown file type $file"
    esac

    # Remove generated file
    rm -f tests/${basename}.diff tests/${basename}.ll tests/${basename}.exe tests/${basename}.s

done
