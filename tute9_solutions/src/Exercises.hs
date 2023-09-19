{-# LANGUAGE NoImplicitPrelude #-}
module Exercises where

import           Base hiding (maybe)
import           Functor
import           ExercisesW8

-- | Ignores the first value, and puts the second value in a context
--
-- >>> Just 5 $> 1
-- Just 1
-- >>> [1,2,3,4] $> 3
-- [3,3,3,3]
($>) :: Functor f => f b -> a -> f a
($>) = flip (<$)

infix 4 $>
