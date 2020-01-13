# Prolog Intepreter
The motivation here was to learn the new Scala 3 syntax and implement a prolog interpreter with pure functional code.
I am not attempting to implement the entire prolog syntax, but rather implement a minimal set and aim for a neat solution.

For this implementation I have used the following:

* Scala 3 given clauses for implicit parameters
* Scala 3 enum's for the ADT
* ZIO Core for managing effects
* ZIO Streams to implement an iterative prompt for each solution result
* Scala 3 new control syntax and use indenting to get more readable code
* Scala 3 Top-level definitions for functional methods
* Scala Meta library for parsing the clauses
* Scopt library for parsing command line arguments

## TODO
There are few things that would be interesting to implement, such as:
* cut (!)
* not (~) predicate
* numbers incl. numeric operations
* more...

# Example
```
planet(mercury)
planet(venus)
planet(earth)
planet(mars)
planet(jupiter)
planet(saturn)
planet(uranus)
planet(neptune)

nextTo(mercury,venus)
nextTo(venus,earth)
nextTo(earth,mars)
nextTo(mars,jupiter)
nextTo(jupiter,saturn)
nextTo(saturn,uranus)
nextTo(uranus,neptune)

isNextTo(X,Y) := nextTo(X,Y)
isNextTo(X,Y) := nextTo(Y,X)

bigger(jupiter,saturn)
bigger(saturn,neptune)
bigger(neptune,earth)
bigger(earth,mars)

isBigger(X,Y) := bigger(X,Y)
isBigger(X,Y) := bigger(X,Z) && isBigger(Z,Y)

```

# Commandline Query
```bash
$ echo "isBigger(A,B)" | sbt "run ./src/main/resources/test.txt"
```

The following output will be generated:
```
A=jupiter, B=saturn,
A=saturn, B=neptune,
A=neptune, B=earth,
A=earth, B=mars,
A=jupiter, B=neptune,
A=jupiter, B=earth,
A=jupiter, B=mars,
A=saturn, B=earth,
A=saturn, B=mars,
A=neptune, B=mars
```