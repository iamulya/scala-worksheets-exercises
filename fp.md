###Interesting points:
1. Functions are first class values in fp, however they cant be compared for equality

###Algebra
Algebra in the mathematical sense of one or more sets, together with a collection of functions
operating on objects of these sets, and a set of axioms. Axioms are statements assumed true from which we can
derive other theorems that must also be true. In case of Functional programming, the sets are replaced by different types, and the collection of functions on set by the functions defined on
the type.

###Alternative interpretation of Algebra
1. Naively speaking algebra gives us the ability to perform calculations with numbers and symbols.
Abstract algebra treats symbols as elements of a vector space: they can be multiplied by scalars and added to each other.
But what makes algebras stand apart from linear spaces is the presence of vector multiplication: a bilinear product of vectors whose result is another vector
(as opposed to inner product, which produces a scalar).
Complex numbers, for instance, can be described as 2-d vectors, whose components are the real and the imaginary parts.
2. An algebra is a generalization of algebraic structures like groups, rings, monoids etc.
An algebra, then, is just a type τ with some functions and identities. These functions take differing numbers of
arguments of type τ and produce a τ: uncurried, they all look like
``` haskell
(τ, τ,…, τ) → τ or τⁿ → τ
```
They can also have "identities"—elements of τ that have special behavior with some of the functions.
3. An algebra is just a common pattern in mathematics that's been "factored out", just like we do with code.
People noticed that a whole bunch of interesting things—the aforementioned monoids, groups, lattices and so on—all follow a similar pattern, so they abstracted it out.
The advantage of doing this is the same as in programming: it creates reusable proofs and makes certain kinds of reasoning easier.

###API as an algebra
Approaching API as an algebra is writing down the type
signature for an operation we want, and then “following the types” to an implementation.
When working this way, we just focus on lining up
types. This isn’t cheating; it’s a natural style of reasoning, analogous to the reasoning
one does when simplifying an algebraic equation. We’re treating the API as an algebra,
or an abstract set of operations along with a set of laws or properties we assume to be
true, and simply doing formal symbol manipulation following the rules of the game
specified by this algebra.

###F-Algebra
Explanations:
1. F-Algebra can perhaps be seen as abstracting out algebra/generalizing algebra. An algebra is a type with many functions, in F-Algebra we abstract these functions out into one function.
For example, for a monoid the single function can be represented as:

``` haskell
op ∷ Monoid τ ⇒ Either (τ, τ) () → τ
op (Left (a, b)) = mappend (a, b)
op (Right ())    = mempty
```

This lets us talk about algebras as a type τ with a single function from some combination of Eithers to a single τ.
We can also write this monoid as a new datatype that takes in a monoid as argument.

``` haskell
data MonoidArgument τ = Mappend τ τ -- here τ τ is the same as (τ, τ)
                      | Mempty      -- here we can just leave the () out
```

This new type also turns out to be a functor.
``` haskell
instance Functor MonoidArgument where
  fmap f (Mappend τ τ) = Mappend (f τ) (f τ)
  fmap f Mempty        = Mempty
```

The same can be done for other algebras. Now algebra is just some type τ with a function f τ → τ for some functor f. In fact, we could write this out as a typeclass:
``` haskell
class Functor f ⇒ Algebra f τ where
  op ∷ f τ → τ
```

The F in F-Algebra comes from the functor f.

2.

###F-Coalgebra
 The co implies that it's the "dual" of an algebra—that is, we take an algebra and flip some arrows. I only see one arrow in the above definition, so I'll just flip that:
``` haskell
class Functor f ⇒ CoAlgebra f τ where
  coop ∷ τ → f τ
```
So algebra is just a type τ with a function f τ → τ and a coalgebra is just a type τ with a function τ → f τ.

###Monad
 A monad is just like a monoid, except instead of having a type we have a functor. It's the same sort of algebra, just in a different category.

###Curry-Howard isomorphism
The Curry-Howard isomorphism states that types are isomorphic to theorems and programs are isomorphic to proofs,
so whenever we have a type and a program implementing that type the program is a proof of whatever theorem the type represents.
Usually a type is much more general than the particular program we're writing so the type doesn't capture the entire semantics of our program.
In a sense this is not good situation, because our program depends on a stricter semantics than the types give,
but I'll ignore this conundrum for now and instead focus on the types as theorems bit.

###Parametricity theorem
The main theorem of parametricity is the following:

``` haskell
if f :: t then f ℛ(t) f
```

When t is a closed type, ℛ(t) is a relation between two terms of type t
(we shall see later that the type of ℛ is actually slightly more general).
 In words, parametricity states that any term f of type t is related to itself by ℛ(t).
 One virtue of this parametricity is that we can infer a significant number of things that do not occur.
 This theme of learning what does not occur is ubiquitous when deploying these programming techniques.

 More about [Parametricity](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/)

###Parametric polymorphism
Parametric polymorphism means that a function will act on all types uniformly.

#####Most functional languages have polymorphism through parametric polymorphism - why?
Because...
1. it allows code to be reused.
2. we can derive useful free theorems. [Free theorems](http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html): a method for
                                       obtaining proofs of program properties from parametrically
                                       polymorphic types in purely functional languages
3. type-agnostic reasoning is better!


