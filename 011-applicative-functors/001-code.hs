-- # Applicative functiors
--
-- ## Functors Redux
--
-- - functors are things that can be mapped over i.e. lists Maybes and trees
--
-- - functors are described by the type class Functor, which has only one type class
--   method: fmap . fmap has a type of fmap :: (a -> b) -> f a -> f b, which says,
--   “Give me a function that takes an a and returns a b and a box with an a (or
--   several of them) inside it, and I’ll give you a box with a b (or several of
--   them) inside it.” It applies the func- tion to the element inside the box.

-- - We can also look at functor values as values with an added context. For in-
--   stance, Maybe values have the extra context that they might have failed. With
--   lists, the context is that the value can actually be several values at once or
--   none. fmap applies a function to the value while preserving its context.

-- - If we want to make a type constructor an instance of Functor , it must have a
--   kind of * -> *, which means that it takes exactly one concrete type as a type
--   parameter. For example, Maybe can be made an instance because it takes one type
--   parameter to produce a concrete type, like Maybe Int or Maybe String. If a type
--   constructor takes two parameters, like Either , we need to partially apply the
--   type constructor until it takes only one type pa- rameter. So we can’t write
--   instance Functor Either where , but we can write instance Functor (Either a)
--   where . Then if we imagine that fmap is only for Either a, it would have this
--   type declaration:

--   fmap :: (b -> c) -> Either a b -> Either a c

--   As you can see, the Either a part is fixed, because Either a takes only one
--   type parameter
