![https://travis-ci.org/NWChen/heckell](https://travis-ci.org/NWChen/heckell.svg?branch=master)

[`heckell`](https://travis-ci.org/NWChen/heckell) is a programming language for set theory.

## Usage

```
$ make
$ ./heckell < (YOURFILE).hck
```

## Example

```
let x: int;
x = 4;

let y: int;
y = 6;

print("Calculating the gcd of \(x) and \(y):");

while x != y do 
	if x > y then
		x = x - y;
	else
		y = y - x;
	end
end

print("\(x)");
```
