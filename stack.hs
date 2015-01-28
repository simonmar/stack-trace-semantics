{-# LANGUAGE CPP #-}
module Main where

import Test.QuickCheck
import Control.Monad

data Stack x = E | Stack x :> x
  deriving (Show, Eq)

infixl 6 :>
infixl 5 `elemstack`

x `elemstack` E = False
x `elemstack` s :> x' | x == x'   = True
                      | otherwise = x `elemstack` s

toList x = toList' x []

toList' E        xs = xs
toList' (s :> x) xs = toList' s (x:xs)

fromList xs = foldr (flip (:>)) E (reverse xs)

pprStack (E :> x) = x
pprStack E = "|"
pprStack (s :> x) = pprStack s ++ " > " ++ x

infixl 5 +++

call = (+++)

-----------------------------------------------------------------------------
-- RECURSION_DROPS

-- this satisfies both prop_append1 and prop_append2, but has the
-- undesirable property that the element at the top of the stack is
-- not necessarily the most recently-pushed thing, which we want for
-- profiling.

#if 0
s +++ E      = s
s +++ t :> x = (s +++ t) `push` x

s `push` x  | x `elemstack` s  = s
            | otherwise        = s :> x
#endif

-----------------------------------------------------------------------------
-- RECURSION_TRUNCATES

-- this violates both prop_append1 and prop_append2

#if 0
s +++ E      = s
s +++ t :> x = (s +++ t) `push` x

s `push` x  | Just s' <- dropstack s x = s'
            | otherwise = s :> x

dropstack E x = Nothing
dropstack (s :> x') x | x' == x   = Just (s :> x)
                      | otherwise = dropstack s x
#endif

-----------------------------------------------------------------------------
-- RECURSION_TRUNCATES with prefix-matching in "call"

-- satisfies prop_append2a

#if 1
s +++ t = s `append` t_rest
   where
      (common, t_rest) = match_common_prefixes s t

s `append` E        = s
s `append` (t :> x) = (s `append` t) `push` x

s `push` x  | Just s' <- dropstack s x = s'
            | otherwise = s :> x

dropstack E x = Nothing
dropstack (s :> x') x | x' == x   = Just (s :> x)
                      | otherwise = dropstack s x

match_common_prefixes :: Stack String -> Stack String -> (Stack String, Stack String)
match_common_prefixes s t = (fromList pref, fromList rest)
  where
     (pref, rest) = match sl tl []
     sl = toList s
     tl = toList t

match []       []       acc = (acc, [])
match xs       []       acc = (acc, [])
match []       ys       acc = (acc, ys)
match (x : xs) (y : ys) acc
  | x == y              = match xs ys acc
  | otherwise = (acc, y:ys)
#endif

-----------------------------------------------------------------------------

#if 0
s +++ t = s `append` t

s `append` E        = s
s `append` (t :> x) = (s `append` t) `push` x

--
--
-- s +++ t  =  s `append` t_rest
--   where
--      (common, t_rest) = match_common_prefixes s t
-- 
-- match_common_prefixes :: Stack String -> Stack String -> (Stack String, Stack String)
-- match_common_prefixes s t = (fromList pref, fromList rest)
--   where
--      (pref, rest) = match sl tl []
--      sl = toList s
--      tl = toList t
--
-- match []       []       acc = (acc, [])
-- match xs       []       acc = (acc, [])
-- match []       ys       acc = (acc, ys)
-- match (x : xs) (y : ys) acc
--   | x == y              = match xs ys acc
--   | x == ".."           = case xs of
--                             []       -> (acc, y:ys)
--                             (x':xs') -> dotdot x' xs' ys acc
--   | otherwise = (acc, y:ys)
-- 
-- dotdot x xs []     acc = (acc, [])
-- dotdot x xs (y:ys) acc
--   | x == y    = match xs ys (x:acc)
--   | otherwise = dotdot x xs ys acc

s `push` ".."  =  s
s `push` x  =  rec s x :> x

rec E x = E
rec (s :> y) x
  | y == x    =  s >>> ".."
  | otherwise =  (rec s x) >>> y

E            >>> x     = E :> x
(s :> "..")  >>> ".."  = s :> ".."
(s :> x)     >>> y     = s :> x :> y
#endif

-----------------------------------------------------------------------------

#if 0
type VStack x = (Stack x, Maybe x)

(s,_) `push` x  | x `elemstack` s = (s,      Just x)
                | otherwise       = (s :> x, Nothing)

(s, _) +++ (t, Nothing) = s `append` t
(s, _) +++ (t, Just x)  = (s `append` t) `push` x

s `append` E        = (s, Nothing)
s `append` (t :> x) = (s `append` t) `push` x

prop_append1 = forAll vstacks $ \s ->
               forAll vstacks $ \t ->
               forAll Main.labels $ \x ->
                      s +++ (t `push` x) == (s +++ t) `push` x

prop_append2 = forAll vstacks $ \s ->
               forAll Main.labels $ \x ->
                      (s `push` x) +++ s == s `push` x

vstacks :: Gen (VStack Char)
vstacks = sized $ \n ->
    do k <- choose (0,n)
       l <- sequence [ Main.labels | _ <- [1..k] ]
       return (foldl (:>) E l, Nothing)
#endif

-----------------------------------------------------------------------------

#if 1
prop_append1 = forAllShrink stacks shrinkstack $ \s ->
               forAllShrink stacks shrinkstack $ \t ->
               forAll Main.labels $ \x ->
                      call s (push t x) == push (call s t) x

prop_append1a = forAllShrink stacks shrinkstack $ \s ->
                forAllShrink stacks shrinkstack $ \t ->
                forAll Main.labels $ \x ->
                       x `elemstack` s ||
                       call s (push t x) == push (call s t) x

prop_append2 = forAllShrink stacks shrinkstack $ \s ->
               forAll Main.labels $ \x ->
                      call (s `push` x) s == s `push` x

-- much easier to satisfy:
prop_append2a = forAllShrink stacks shrinkstack $ \s ->
                forAll Main.labels $ \x ->
                      x `elemstack` s ||
                      call (s `push` x) s == s `push` x

prop_append3 = forAllShrink stacks shrinkstack $ \s ->
               forAll Main.labels $ \x ->
               forAll (Main.labels `suchThat` (/= x)) $ \y ->
                      (s `push` x) +++ (s `push` y) == (s `push` x) `push` y

shrinkstack E = []
shrinkstack (s :> x) = s : shrinkstack s

stacks = sized $ \n ->
    do k <- choose (0,n)
       l <- sequence [ Main.labels | _ <- [1..k] ]
       return (foldl push E l)
--       return (foldl (:>) (E :> "..") l)
#endif

labels = elements (map (:[]) ['a'..'e'])

tests = do
  quickCheck prop_append1
  quickCheck prop_append2

-- let
--   f = scc "a" \x . e
-- in
--   scc "a" scc "b" ... f ...
