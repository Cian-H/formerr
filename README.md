# Formerr

Formerr (the FORtran Monadic ERRor system) is a library aiming to bring the benefits of
functional-style monadic error handling to the fortran ecosystem. Currently, the standard approach
to error-handling in fortran involves the passing of status codes in a style similar to C or
golang. While this works it results in a per-function error contract, which can create problems
in systems where a consistent, predictable, and enforced error handling pattern are required, e.g:

- Highly modular systems
- Lazy executing libraries
- Distributed systems

Formerr aims to solve this by creating a simple error monad for fortran, along with accompanying
functions and error handling logic to create a clean, consistent, and predictable error handling
paradigm, more akin to functional languages where these systems are often easier to implement.
