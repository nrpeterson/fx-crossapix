# fx-crossapix

In this repository, I am building an (almost) purely functional library for building
applications representing Conceptis Puzzles' single-clue Cross-a-Pix puzzle. (Much of
the remaining work is in reducing that 'almost' caveat, particularly in the board parser.)

## About Cross-a-Pix

[Conceptis Puzzles] released [Cross-a-Pix] in 2015, based 
on [Tilepaint] by [Nikoli Puzzles].

In Single-Clue Cross-a-Pix, you are given a grid in which grid spaces are combined into regions and each row/column has
a desired total.  The goal is to reveal a picture by coloring each region black or white such that the number of black
spaces in row/column matches that total.

![A Conceptis Single-Clue Cross-a-Pix puzzle][screenshot]

(Screenshot from [Conceptis Puzzles Single-Clue Cross-a-Pix # 32090100378][puzzle-link].
Used with permission -- my thanks to the team at Conceptis Puzzles!)

## About This Project

### Why Am I Doing This?

Logic puzzles of all stripes are one of my passions.  I frequently find myself writing small snippets of code here and
there to help with solutions.

In this project, my primary aim was to write code that solves these Single-Clue Cross-a-Pix puzzles the same way that I 
do, and explains the solution the same way that I would.  In part, that aim was inspired by my current efforts to teach
my 4-year old to love puzzles like I do.

Aside from that, I love writing Scala and am a functional programming enthusiast in general.  Having recently spent 
some time in Haskell, I wanted to get my hands dirty with much of the more functional-leaning Scala ecosystem (such as
[Cats][Cats] and [Cats-Effect][Cats-Effect]). This aim, in turn, lead to a really interesting encapsulation of the 
different parts of the problem, which lead to a much more interesting project than it would have bene otherwise!

### Overall Design

To build a Cross-a-Pix application, you need to encapsulate:
- Application logic
- Solver logic
- Game structure and logic

My goal in this project has been to keep these three as absolutely separate as possible:
- Application logic is encoded in the [CAPApp trait / bivariate typeclass][CAPApp] in the form of monadic actions for
  specific pieces of the game loop.
- Solver logic is encoded in types that satisfy the [Heuristic typeclass][heuristic], with a modular attribute 
  dependency management system encoded by the [Attribute][attribute] and [Attributes][attributes] typeclasses and the
  ability to chain multiple heuristics together to form a new heuristic.
- The [CAPApp.runApp][CAPApp] method then encodes the game logic, and combines a `CAPApp` and a `Heuristic` (and its 
  backing attributes) into a running application.

Additionally: because the structure of these puzzles is a royal pain to encode by hand (particularly for the large 
ones!), I wrote a [board reader][BoardReader] with home-brewed computer vision / OCR code to parse a puzzle from a 
screenshot from Conceptis' online play site (such as the above).

### Parsing Boards from Screenshots

### Applications

### Attributes

### Heuristics

## Todos...
\#irony

## Acknowledgements

I would like to thank [Conceptis Puzzles]. Not only have they provided me untold hours of entertainment over 
the past 20 years, but they have graciously agreed to permit me to include some screenshots of their puzzles in this 
codebase for tests of the board parser and examples of use.  Thank you for everything you do!

[attribute]: src/main/scala/dev/nrp/puzzles/crossapix/attributes/Attribute.scala "Attribute.scala"
[attributes]: src/main/scala/dev/nrp/puzzles/crossapix/attributes/Attributes.scala "Attributes.scala"
[BoardReader]: src/main/scala/dev/nrp/puzzles/crossapix/vision/BoardReader.scala "BoardReader.scala"
[CAPApp]: src/main/scala/dev/nrp/puzzles/crossapix/app/CAPApp.scala "CAPApp.scala"
[Cats]: https://typelevel.org/cats/ "Typelevel Cats"
[Cats-Effect]: https://typelevel.org/cats-effect/ "Typelevel Cats-Effect"
[Conceptis Puzzles]: https://conceptispuzzles.com "Conceptis Puzzles"
[Cross-a-Pix]: https://conceptispuzzles.com/index.aspx?uri=puzzle/cross-a-pix "Conceptis Cross-a-Pix"
[heuristic]: src/main/scala/dev/nrp/puzzles/crossapix/heuristics/Heuristic.scala "Heuristic.scala"
[Nikoli Puzzles]: https://www.nikoli.co.jp/en/puzzles/ "Nikoli Puzzles"
[puzzle-link]: https://www.conceptispuzzles.com/index.aspx?uri=puzzle/euid/0100000047204f2ce02e175278fa54852890dcde1b13c39313967460a20ec1229f6ffad1b144f8c5b933f1ca85ee3d545dc1c430/play "Conceptis Cross-a-Pix # 32090100378"
[screenshot]: src/test/resources/32090100378.png "Screenshot of a Conceptis Single-Clue Cross-a-Pix puzzle"
[Tilepaint]: https://nikoli.co.jp/en/puzzles/tilepaint/ "Nikoli Tilepaint"
