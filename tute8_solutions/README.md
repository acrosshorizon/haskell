<<<<<<< HEAD
---
documentclass: tufte-handout
title: Functor and Applicative
author: FIT2102 Programming Paradigms
date: S2 2017 Week 8
fontsize: 12pt
# Comment or change these if you don't have these fonts installed
fontfamily: libertine
fontfamilyoptions: mono=false
monofont: inconsolata
newtxmathoptions:
- cmintegrals
- cmbraces
- libertine
colorlinks: true
linkcolor: RoyalBlue
urlcolor: RoyalBlue
---

# Preamble

This week, we attack *real* functional programming: Functors and
Applicatives. Think of them of generalisation of higher
functions. Functors and applicatives are two of the main building
blocks of functional programming theory.

Functor and Applicative are *typeclasses* like we saw last week. That
is they are properties you apply on types. Types, by themselves,
cannot enforce certain properties it is therefore the programmer's
task to implement them.

When we implemented other typeclasses we implicitly used a concept
called "minimal complete definition." That is, the minimum definition(s) -- or
function(s) -- that are needed to express a property.[^1] Functor and
Applicative are no different.

[^1]: E.g., `Eq` needs only `==`, `Ord` can be expressed with
    `compare` or `Eq` and `<`, etc.

# Functor

![Functor application](functor.png "fmap")

## Minimal complete definition

The minimal definition of a functor is `<$>` (fmap[^2]).

```haskell
(<$>) :: (a -> b) -> f a -> f b
```

[^2]: Note that `fmap` stands for "functor map" because `map` was
    already reserved, it is not `flatMap`.

## Laws

All instances of the `Functor` type-class must satisfy two laws. These
laws are not checked by the compiler. These laws are given as:

 1. The law of identity [^3]
    $$ \forall x: (\text{id <\$> } x) \equiv x $$
 2. The law of composition
    $$ \forall f, g, x:
    (f \circ g \text{ <\$> } x) \equiv (f \text{ <\$> } (g \text{ <\$> } x)) $$

So a functor takes a function, an element in a context, applies the
function to the element, and returns the result in the context.[^4]

[^3]: Where `id` is the identity function.

[^4]: If you replace `f` with `[]` you will notice that `fmap` is a
    general version of `map`; in a way, a type implementing functor is
    a type over which you can map.

# Applicative

![Using apply](applicative.png "apply")

## Minimal complete definition

The minimal definition of an applicative functor is `pure` and `<*>` (apply):
```haskell
pure  :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
```

## Laws

All instances of the `Applicative` type-class must satisfy four laws.
These laws are not checked by the compiler. These laws are given as:

 1. The law of left identity
    $$ \forall x: \text{pure id <$\ast$> } x \equiv x $$
 2. The law of composition
    $$ \forall a, b, c:
    ((\circ) \text{ <\$> } a \text{ <$\ast$> } b \text{ <$\ast$> } c)
    \equiv (a \text{ <$\ast$> } (b \text{ <$\ast$> } c)) $$
 3. The law of homomorphism
    $$ \forall f, x: \text{pure } f \text{ <$\ast$> pure } x \equiv \text{ pure } (f\ x) $$
 4. The law of interchange
    $$ \forall x, y: y \text{ <$\ast$> pure } x \equiv \text{ pure } y \text{ <$\ast$> } x $$

So, an applicative takes a function within a context, an element
within the same context, applies the function, and returns the results
in the context.

# Exercises

Now that the fundamentals are out of the way, you will implement some
functions that leverage the functor and applicative typclasses:

 1. `lift`, aka the *cheat function*. `lift` takes a binary function
    and applies it to two elements wrapped in a context.
 2. If you had time, you implemented `flipMaybe` last week. It has one
    major shortcoming: it is not general. Using `<$>` and `<*>` you
    can write a general function that takes a list of wrapped elements
    and returns a wrapped list of elements.[^5]
 3. `replicate` takes an element in a context and replicates the
    effect a given number of times.
 4. `filtering` is a compound filter function which takes a predicate
    that also produces an effect.

[^5]: Think about it as a way to automatically handle failures.

## Optional

What does the following code produce?
```haskell
filtering (const $ [True, False])
```

# Credits

Images from [adit.io](http://adit.io/) in [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html).
=======
[Worksheet](https://docs.google.com/document/d/1cUGRjx9ep3MzmRgB_0H_3bHD704GmmwTwMPqQVZCPdw/edit#bookmark=id.az76vi2swwc)
>>>>>>> b518623a426a044cb2f02eb543e1c69babf6f609
