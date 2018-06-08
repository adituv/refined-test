{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Demonstration of interval refinements in Haskell itself, derived
--   from the reddit post <https://redd.it/8pj3na/>
module Main where

import GHC.TypeLits
import Refined

-- | Refined version of 'log'; type is equivalent to
--   @(0,+inf) -> (-inf,+inf)@ in standard interval arithmetic.
log' :: (Ord a, Floating a) => Refined (GreaterThan 0) a -> a
log' (unrefine -> x) = log x

-- | Refined version of @absP1@ from the linked reddit post.
--   The type is equivalent to @(-inf,+inf) -> [1,inf)@ in
--   standard interval arithmetic.
absP1 :: (Ord a, Num a) => a -> Refined (From 1) a
absP1 x = unsafeRefine $ 1 + abs x

-- | Here we combine the two.  As the intervals do not match up exactly,
--   we have to change the refinement, which we here do with the 'weaken'
--   function.
comb :: (Ord a, Floating a) => a -> a
comb = log' . weaken . absP1

-- For some reason, this instance is missing from Refined?  Maybe it
-- is unsafe in some circumstances?

-- | This instance is an assertion that it is safe to weaken the refinement
-- @From m@ to @GreaterThan n@ provided that @m@ is strictly greater than @n@.
instance ((m <=? n) ~ False) => Weaken (From m) (GreaterThan n)

main :: IO ()
main = print $ comb (-5)
