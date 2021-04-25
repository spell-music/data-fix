data-fix - fixpoint types and recursion schemes
==============================================================

Fixpoint types and recursion schemes. If you define your AST as
fixpoint type, you get fold and unfold operations for free.

```haskell
Fix f = f (Fix f)
```

Type ``f`` should be a ``Functor`` if you want to use simple 
recursion schemes or 'Traversable' if you want to use monadic recursion schemes. 
This style allows you to express recursive functions in non-recursive manner.
You can imagine that a non-recursive function holds values of the previous iteration.

Little example:

```haskell
type List a = Fix (L a)

data L a b = Nil | Cons a b

instance Functor (L a) where
  fmap f x = case x of
    Nil      -> Nil
    Cons a b -> Cons a (f b)

length :: List a -> Int
length = cata $ \x -> case x of
  Nil      -> 0
  Cons _ n -> n + 1

sum :: Num a => List a -> a
sum = cata $ \x -> case x of
  Nil      -> 0
  Cons a s -> a + s
```

### Acknowledgements

Thanks for contribution to: Matej Kollar, Herbert Valerio Riedel

