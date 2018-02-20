#!/bin/bash

HECKELL="./heckell"

# Set time limit for all operations
ulimit -t 30

files="tests/test-*.hck tests/fail-*.hck"

for file in $files
do
    basename=`echo $file | sed 's/.*\\///
                         		s/.hck//'`
	echo "$basename..."

    case $file in
	*test-*)
	    $HECKELL < $file;;
	*fail-*)
	    $HECKELL < $file;;
	*)
	    echo "unknown file type $file"
    esac

done