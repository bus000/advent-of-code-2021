{-# LANGUAGE BangPatterns #-}
module Util
    ( converge
    ) where

converge :: Eq a => (a -> a) -> a -> a
converge f (!x) = if f x == x then x else converge f $ f x
