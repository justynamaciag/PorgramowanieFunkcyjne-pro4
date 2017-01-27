Categories

A category is a natural extension of our notion of sets and functions. The generalization of a set in a category 
is called an object (a pretty neutral term with little semantic ballast), and the generalization of a function is
 called a morphism. In fact, the standard example of a category is the category of sets and functions called (capital letter) Set.

A morphism (read “function”) goes from one object (read “set”) to another. Mathematical functions like sin or exp 
usually go from the set of real numbers to the set of real numbers. But you may also define functions like isPrime
 that go from natural numbers to Booleans, or a function price that goes from a set of goods to the set of numbers.

The only thing a mathematician needs to know about morphisms is that they can be composed. If you have a morphism
 from A to B, A->B, and another going from B to C, B->C, then they can be composed to a morphism from A to C, A->C.
 And just like the standard composition of functions, morphism composition must be associative, so we don’t need
 parentheses when composing more than two of them.

Actually, two things. There must be, for every object, a special morphism called identity that essentially does
 nothing and when composed with any other morphism reproduces the same morphism.