unicornfart: A simple rainbow table generator implemented in Haskell
====================================================================

Note
----
This software is not complete but is a fair start.

Depends
-------
Depends on PureMD5.  This can be obtained via cabal

    cabal install pureMD5

Build
-----
Compile with your choice of Haskell compiler, for instance:

    ghc unicornfart.hs

Run
---
The only argument is the starting plaintext which is then reudced.  The result
is a list of starting plaintext to hash endpoint.
    ./unicornfart plaintext

Contact
-------
Matt Davis
mattdavis9@gmail.com
