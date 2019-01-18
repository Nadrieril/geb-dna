# Introduction

In chapter 16 of Gödel, Escher, Bach, the author presents a toy model of DNA and the following challenge:
devise a self-replicating strand.

This repo contains some code I used to play with the toy model.

# Rules

The model has two objects: strands of DNA and enzymes.
A strand of DNA is a sequence of the characters 'A', 'C', 'G' and 'T' (called 'bases').

Enzymes are small programs that act on strands by moving around it and editing the bases of the strand.
An enzyme is in effect a sequence of instructions called amino acids (see the `AminoAcid` datatype, and the `execAA` function).

The interesting bit is that a strand can be converted into one or more enzymes by interpreting
every pair of bases as an amino acid (see the `dupletToAA` and `synthesize` functions)

Given some initial strands, one can transform some of them into enzymes, use those enzymes to act on the strands to form new or modified strands,
and repeat.
The objective is to design a self-replicating strand, which means a strand such that,
after some sequence of the above cycle, we end up with at least two copies of the initial strand.

There are some additional rules in the book, that I have implemented but not detailed here.


# Results

I have found one such self-replicating strand: `CGTCTCTCTATAGAGAGACG`.
See `test.hs` for the reasoning that lead to finding this strand.
