module MINQ
  ( MINQ (..),
    runMINQ,
    select_,
    join_,
    where_,
    and_,
    or_,
  )
where

import           Control.Applicative
import           Control.Monad

-- Extract fields from a row.
select_ :: (Monad m) => (a -> b) -> m a -> m b
select_ = fmap

-- Combine two rows when they have a value in common.
join_ ::
  (Monad m, Alternative m, Eq c) =>
  m a ->
  (a -> c) ->
  m b ->
  (b -> c) ->
  m (a, b)
join_ m1 f1 m2 f2 = do
  a <- m1
  b <- m2
  guard $ f1 a == f2 b
  return (a, b)

-- Filter rows based on a predicate.
where_ :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
where_ fn vals = do
  val <- vals
  guard $ fn val
  return val

-- Combine two prediciates. The output is true if both predicates return true.
and_ :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f `and_` g = \x -> f x && g x

-- Combine two prediciates. The output is true if either predicate returns true.
or_ :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f `or_` g = \x -> f x || g x

-- Describes how to run a MINQ query.
minq_ :: (b -> c) -> a -> (a -> b) -> c
minq_ selectFn joinFn whereFn = (selectFn . whereFn) joinFn

-- A wrapper that allows for specifying a query without a where clause.
data MINQ m a b = MINQ (m a -> m b) (m a) (m a -> m a)
    | MINQ_ (m a -> m b) (m a)

-- Runs MINQ queries.
runMINQ :: (Monad m, Alternative m) => MINQ m a b -> m b
runMINQ (MINQ s j w) = minq_ s j w
runMINQ (MINQ_ s j)  = minq_ s j (where_ $ const True)
