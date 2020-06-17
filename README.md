# tills
A toy programming language built in Haskell. This was originally written as a part of a course on programming language semantics. For the course version I also wrote a static analysis tool which used [symbolic execution](https://en.wikipedia.org/wiki/Symbolic_execution) to automatically detect errors in Tills source code.

Since the course I have added some features to this language, such as the ability to perform IO. I used this feature to solve some basic programming problems on [Kattis](https://www.kattis.com/). Because Tills is so small, I was able to upload the entire Haskell interpreter as well as the the Tills source code for the programming puzzle I was solving :-).

The implementation is inspired by the chapter on monadic parsing in Graham Hutton's [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html) and the semantics of the "While" programming language outlined in Nielson and Nielson's [Semantics with Applications: An Appetizer](http://www.springer.com/la/book/9781846286919)
