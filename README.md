![https://travis-ci.org/NWChen/heckell](https://travis-ci.org/NWChen/heckell.svg?branch=master)

[`heckell`](https://travis-ci.org/NWChen/heckell) is a programming language for set theory.

## Usage

```
$ make
$ ./heckell < (YOURFILE).hck
```

## Example

```
let S1 : int set;
S1 = { 1, 2, 2 };

print("Creating set S1 = {1, 2, 2}");

let S2 : int set;
S2 = { 2, 8, 9 };

print("Creating set S2 = {2, 8, 9}");

let S3 : int set;
S3 = S1 + S2;
print("S1 + S2 ==> \(S3)");

let S4 : int set;
S4 = S2 - S1;
print("S2 - S1 ==> \(S4)");
```
