#Category Theory for computer science
- study of collections of concepts and arrows

concepts = `type`
arrows   = morphism between concepts(something that converts 1 type to another)
morphism = `function` defined against 2 `type`s
category = grouping of concepts and arrows

For example: Category of Cats as well as the captions to convert between serious cat and lol cat

Category Theory is the study of categories and relationships between them

Most used categories in programming: `type`s

```scala
    def lift3[A,B,C,D](f: Function3[A,B,C,D]) = {
      (oa: Option[A], ob: Option[B], oc: Option[C]) =>
        for(a <- oa; b <- ob; c <- oc) yield f(a,b,c)
    }
```

#Functor
- transformation from one category to another
morphism - would be a box to convert dim cats to glowing cats
For example:
- A functor would convert a cat into a dog. Dim cat into Dog. Glowing cat into glowing dog.
- A functor could _also_ convert the box so that it can convert dim dogs into glowing dogs
- For the transformation to be a functor transformation:
1. All morphisms must be preserved in the transformation - if we have a function
that manipulates types in the first category, we should have a transformed function that operates on teh transformed types.

#Monad
- means of combining a functor application, if it is an endofunctor
endofunctor - functor that converts concepts and morphisms in its category back into same category
For example: Transforming a cat more than once by the same functor could be reduced into single functor application.

#Currying
- taking a function of several arguments and
turning it into a function that takes a single argument and
returns a function that takes the next argument and so on
until finally one of the functions returns a value

#Applicative
- Takes a function inside a Functor and a value inside a Functor and returns the result Functor (applicative functor)

#Applicative Style
- Lifting functions into Applicative Functors
- Can be used to simplify the construction of complex function dependencies. Keeps the values inside an applicative functor
- Applicative functors provide a way to take two computations and join them together using a function

