#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: ./heckell.sh YOURFILE.hck"
  exit 1 
fi

/heckell < $1 | cat > temp.ll
llc temp.ll > temp.s
cc -o temp.exe temp.s
./temp.exe
