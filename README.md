# Learning Haskell

My way of learning FP through `Haskell`

## Logical Puzzles and General examples in pure Haskell

This is just a repository containing `Haskell` solutions to generic problems
and Logical Puzzles from various sources. In particular non-determinism using
the list monad of `Haskell` has been extensively used across the Puzzles for
solving the Logical Puzzles.

Of particular interest would be the Logic puzzles from Chapter 4 of `SICP` -
*Structure and Interpretation of Computer Programs*

## Running

Since each of the Haskell modules have individual problems and puzzles, as of
now there is no generic `main` method which could run everything in one shot,
but the test cases and a common run command will be added soon.

## Sample executions

> From `SICP 4.3.2 Examples of Nondeterministic Programs` multiple-dwelling
> puzzle as solved in `HsStudy11.hs`

``` haskell
λ> solution
Mr."Baker" lives on the floor no.3
Mr."Cooper" lives on the floor no.2
Mr."Fletcher" lives on the floor no.4
Mr."Miller" lives on the floor no.5
Mr."Smith" lives on the floor no.1
```

> From `SICP 4.4.2` Liars puzzle as solved in `HsStudy13.hs`

``` haskell
λ> ranks
"Betty" got a rank 3
"Ethel" got a rank 5
"Joan" got a rank 2
"Kitty" got a rank 1
"Mary" got a rank 4
```

> From `SICP 4.4.3` Daughter's puzzle as solved in `HsStudy14.hs`

``` haskell
λ> solutionSet
"Mr.Moore" is the father of "Mary" and his yacht name is "Lorna"
"Colonel Downing" is the father of "Lorna" and his yacht name is "Melissa"
"Mr.Hall" is the father of "Gabrielle" and his yacht name is "Rosalind"
"Sir Barnacle Hood" is the father of "Melissa" and his yacht name is "Gabrielle"
"Dr.Parker" is the father of "Rosalind" and his yacht name is "Mary"
```

## References
* [SICP] - The classical SICP
* [Functional Programming] - Functional Programming by Example!
* [Exploring Languages with Interpreters and Functional Programming] - Learn FP!
* [Elisp Programming] - A wonderful overview of elisp!
* [SoH] - School Of Haskell
* [LYAH] - The ever popular Learn You A Haskell

[SICP]: <http://web.mit.edu/alexmv/6.037/sicp.pdf>
[Functional Programming]: <http://caiorss.github.io/Functional-Programming/>
[Exploring Languages with Interpreters and Functional Programming]: <https://john.cs.olemiss.edu/~hcc/csci450/ELIFP/ExploringLanguages.html>
[Elisp Programming]: <http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html>
[SoH]: <https://www.schoolofhaskell.com>
[LYAH]: <http://learnyouahaskell.com/>

#### license

MIT
