------------------------------------------------------------------------------
--- Library defining natural numbers in Peano representation and
--- some operations on this representation.
---
--- @author Michael Hanus
--- @version May 2017
--- @category general
------------------------------------------------------------------------------

module Nat
  ( Nat(..), fromNat, toNat, add, sub, mul, leq
  ) where

--- Natural numbers defined in Peano representation.
data Nat = Z | S Nat
 deriving (Eq,Show)

--- Transforms a natural number into a standard integer.
fromNat :: Nat -> Int
fromNat Z     = 0
fromNat (S n) = 1 + fromNat n

--- Transforms a standard integer into a natural number.
toNat :: Int -> Nat
toNat n | n == 0 = Z
        | n > 0  = S (toNat (n-1))

--- Addition on natural numbers.
add :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)

--- Subtraction defined by reversing addition.
sub :: Nat -> Nat -> Nat
sub x y | add y z == x  = z where z free

--- Multiplication on natural numbers.
mul :: Nat -> Nat -> Nat
mul Z     _ = Z
mul (S m) n = add n (mul m n)

-- less-or-equal predicated on natural numbers:
leq :: Nat -> Nat -> Bool
leq Z     _     = True
leq (S _) Z     = False
leq (S x) (S y) = leq x y
