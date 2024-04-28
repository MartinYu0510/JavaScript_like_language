module Church where

import Prelude hiding (add, and, exp, fst, iszero, mul, not, one, or, snd, three, two, zero)

-- | Church Booleans
--
-- You will see that the above macros for true, false and iff are behaving as expected.
-- For example, one may verify that iff true M N is equivalent to M, and iff false M N is equivalent to N.
--
-- >>> iff true "M" "N"
-- "M"
--
-- >>> iff false "M" "N"
-- "N"
--
-- Boolean arithmetic is also encodable in this way. For example,
-- Boolean negation is essentially flipping the two “branches’ ’:
--
-- >>> iff (not true) "M" "N"
-- "N"
true = \t f -> t

false = \t f -> f

iff = \b t f -> b t f

not b = \t f -> b f t

-- | Church Booleans - Test
--
-- >>> iff (Church.and true false) "get true" "get false"
-- "get false"
--
-- >>> iff (Church.and true true) "get true" "get false"
-- "get true"
--
-- >>> iff (Church.and false true) "get true" "get false"
-- "get false"
--
-- >>> iff (Church.and false false) "get true" "get false"
-- "get false"
--
-- >>> iff (Church.or true false) "get true" "get false"
-- "get true"
--
-- >>> iff (Church.or true true) "get true" "get false"
-- "get true"
--
-- >>> iff (Church.or false true) "get true" "get false"
-- "get true"
--
-- >>> iff (Church.or false false) "get true" "get false"
-- "get false"
--
-- >>> iff (Church.not (Church.or true false)) "get true" "get false"
-- "get false"
and :: (t1 -> t2 -> t3) -> (t4 -> t2 -> t1) -> t4 -> t2 -> t3
and b1 b2 = \t f -> b1 (b2 t f) f

or b1 b2 = \t f -> b1 t (b2 t f)

-- | - Church Pairs
--
--  For pairs, we need three operations mkpair, fst, and snd that obey the following behavior:
--
--  >>> fst (mkpair "M" "N")
--  "M"
--
--  >>> snd (mkpair "M" "N")
--  "N"
mkpair x y = \s -> iff s x y

fst p = p true

snd p = p false

-- | - Church Triples
--
--  >>> fst3 (mktriple "M" "N" "P")
--  "M"
--
--  >>> snd3 (mktriple "M" "N" "P")
--  "N"
--
--  >>> thd3 (mktriple "M" "N" "P")
--  "P"
mktriple :: t1 -> t2 -> t3 -> (t1 -> t2 -> t3 -> t4) -> t4
mktriple x y z = \s -> iff s x y z

fst3 p = p (\x y z -> x)

snd3 p = p (\x y z -> y)

thd3 p = p (\x y z -> z)

-- | Church Numbers
--
-- >>> unchurch (add1 zero)
-- 1
--
-- >>> unchurch (add (church 23) (church 17))
-- 40
--
-- >>> iff (iszero (church 0)) "zero is zero" "zero is not zero"
-- "zero is zero"
church 0 = \f x -> x
church n = \f x -> f (church (n -1) f x)

unchurch cn = cn (+ 1) 0

zero = \f x -> x

one = \f x -> f x

two = \f x -> f (f x)

three = \f x -> f (f (f x))

add1 cn = \f x -> f (cn f x)

add cn cm = cm add1 cn

iszero cn = cn (\x -> false) true

-- | Church Numbers - Multiplications and Exponentiations
--
-- >>> unchurch (mul (church 3) (church 4))
-- 12
--
-- >>> unchurch (exp (church 2) (church 3))
-- 8
mul cn cm = \f x -> cn (cm f) x

exp cn cm = cm cn
