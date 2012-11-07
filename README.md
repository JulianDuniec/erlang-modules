# A collection of erlang-modules.. 

## Erlang N-gram-module
Produces a n-gram graph from a supplied string.

## Example

```` elang

jdngram:create("Hello github! This is an ngram-module written in erlang", 2).
[{" e",1},
 {"ng",2},
 {"it",2},
 {"-m",1},
 {"m-",1},
 {"er",1},
 {"in",1},
 {"ub",1},
 {"Th",1},
 {"th",1},
 {"te",1},
 {"ul",1},
 {"du",1},
 {" i",2},
 {"! ",1},
 {"el",1},
 {"am",1},
 {"ra",1},
 {"b!",1},
 {"la",1},
 {"e ",1},
 {"hu",1},
 {"He",1},
 {" w",1},
 {"an",2},
 {" g",1},
 {"o ",1},
 {[...],...},
 {...}|...]

``````