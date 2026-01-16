## About

This is a compiler that lowers a C-like language into x86 assembly, built (almost) entirely from scratch in Rust. I previously implemented this in C++ for the [Compiler Construction course](https://users.cs.northwestern.edu/~simonec/CC.html) at Northwestern University, but I thought it would be fun to revisit it in Rust this time.

## Language

The IR is split into several layers which get progressively lowered by their respective compilers.

#### L1

Bridge between the higher-level IRs and x86-64 assembly (AT&T syntax). The L1 compiler does a nearly a 1-1 mapping to ASM.

#### L2

Introduces variables. The L2 compiler performs register allocation using the Iterated Register Coalescing algorithm (George and Appel, 1996).

#### L3

Simpler, C-style syntax that eliminates registers and calling convention. The L3 compiler implements instruction selection with the tiling method on trees, using dynamic programming to cover each tree optimally.

#### IR

Explicit control flow to enable easy analysis and transformation. The IR compiler constructs SSA (Cytron et al., 1991) and optimizes the code.

## Acknowledgements

Special thanks to Prof. Campanoni for being an excellent CC instructor and inspiring my love for compilers.
