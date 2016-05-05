# Turing Machine Simulator

This project implements a Turing machine simulator, and includes a number of
Turing machines to demonstrate how it works.

## Turing Machine Conventions

We choose to represent a Turing machine using the `TURING` signature. This is the
best place to start to gain an understanding of the complete system.

For a quick summary, our `TURING` signature sets up the following conventions:

- a tape alphabet that includes a blank symbol
- a two-way infinite tape, represented as a two-way unbounded array
- a state transition function that allows (but does not require) writing and
  moving in the same operation
- a single initial and a single final state

Again, you are encouraged to read the `TURING` signature for more details.

## Building and Running

This code complies cleanly under both SML/NJ and MLton. If possible, we strongly
recommend compiling it using MLton, as it is *orders of magnitude* faster.

### MLton

Once you've installed MLton, building is just one simple command.

You can get usage information by running the command with no arguments.

```console
$ make mlton

$ ./tsim
usage: ./tsim (add|mult|exp) <n> <m>

$ ./tsim add 1 1
[Q0]  11 11
     ^
[Q1]  11 11
      ^
[Q1]  11 11
       ^
[Q1]  11 11
        ^
[Q2]  11111
         ^
[Q2]  11111
          ^
[Q2]  11111
           ^
[Q3]  11111
          ^
[Q4]  1111
         ^
[Q5]  111
        ^
[Q5]  111
       ^
[Q5]  111
      ^
[Q5]  111
     ^
[Q6]  111
     ^
Answer: 2
```


### SML/NJ

The build system isn't quite as polished, but it still works well enough.

```console
$ make smlnj

$ sml -m sources.cm app/call-main.sml
... stuff ...
usage: ./tsim (add|mult|exp) <n> <m>

$ sml -m sources.cm app/call-main.sml add 1 1
... stuff ...
[Q0]  11 11
     ^
[Q1]  11 11
      ^
[Q1]  11 11
       ^
[Q1]  11 11
        ^
[Q2]  11111
         ^
[Q2]  11111
          ^
[Q2]  11111
           ^
[Q3]  11111
          ^
[Q4]  1111
         ^
[Q5]  111
        ^
[Q5]  111
       ^
[Q5]  111
      ^
[Q5]  111
     ^
[Q6]  111
     ^
Answer: 2
```

## Supported Turing Machines

We implement three Turing machines: addition, multiplication, and
exponentiation. The attributions for these machines can be found in
[Credits](#credits). Each has been implemented ascribing to the `TURING`
signature, and resides in `lib/turing/`.


## Credits

- [Addition Machine][adder]. 15453-s16 FLAC, "Lecture: Turing Machines"
- [Multiplication Machine][multiplier]. Turing machine for multiplication.
  YouTube.
- [Exponentiation Machine][exponentiator]. A Turing Machine for Exponential
  Function.

[adder]: http://www.cs.cmu.edu/~flac/PDFs/20-turing.pdf
[multiplier]: https://www.youtube.com/watch?v=fUXYL_hywMo
[exponentiator]: http://philpapers.org/archive/LEMATM

## License

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://jez.io/MIT-LICENSE.txt)

Copyright Jacob Zimmerman. See badge link above for license information.

