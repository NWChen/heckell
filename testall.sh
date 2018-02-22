#!/bin/bash

HECKELL="./toplevel"

# Set time limit for all operations
ulimit -t 30

Compare() {
    if diff $1 $2 &> /dev/null ; then
		echo "SUCCESS"
	else
		echo "FAIL"
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
	    $HECKELL < $file &> "tests/${basename}.diff"
	    Compare tests/${basename}.diff tests/${basename}.out
	    ;;
	*fail-*)
	    $HECKELL < $file &> "tests/${basename}.diff"
	    Compare tests/${basename}.diff tests/${basename}.err
	    ;;
	*)
	    echo "unknown file type $file"
    esac

    # Remove generated file
    rm -f tests/${basename}.diff

done
