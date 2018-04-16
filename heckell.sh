#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: ./heckell.sh YOURFILE.hck"
  exit 1 
fi

./heckell < $1 | cat > temp.ll
llc temp.ll > temp.s
cc -o temp.exe temp.s ut-sets.o
./temp.exe

# basename=`echo $1 | sed 's/.*\\//
#                          s/.mc//'`

# generatedfiles=""

# generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
# Run "./heckell" "$1" ">" "${basename}.ll" &&
# Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
# Run "$CC" "-o" "${basename}.exe" "${basename}.s" "ut-sets.o" &&
# Run "./${basename}.exe" > "${basename}.out"
