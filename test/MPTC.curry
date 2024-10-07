{-# LANGUAGE MultiParamTypeClasses #-}

class MultiParam1 a b where
  foo :: a -> b -> Int

instance MultiParam1 Int Int where
  foo x y = x + y

instance MultiParam1 a a => MultiParam1 [a] [a] where
  foo xs ys = length xs + length ys

operation :: MultiParam1 a b => a -> b -> Int
operation = foo

bar = foo [1 :: Int,2,3] [4 :: Int,5,6]