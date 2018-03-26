#!/usr/bin/env

./heckell < $1 | cat > temp.ll
llc temp.ll > temp.s
cc -o temp.exe temp.s
./temp.exe
