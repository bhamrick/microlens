module Lens.Micro where

import Control.Applicative
import Data.Functor.Identity

type LensLike f s t a b = (a -> f b) -> s -> f t
type LensLike' f s a = LensLike f s s a a

-- The use of s s a a rather than s t a b improves type inference.
view :: LensLike (Const a) s s a a -> s -> a
view l = getConst . l Const
{-# INLINE view #-}

over :: LensLike Identity s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

set :: LensLike Identity s t a b -> b -> s -> t
set l = over l . const
{-# INLINE set #-}

_1 :: Functor f => LensLike f (a, x) (b, x) a b
_1 f (a, x) = fmap (flip (,) x) (f a)
{-# INLINE _1 #-}

_2 :: Functor f => LensLike f (x, a) (x, b) a b
_2 f (x, a) = fmap ((,) x) (f a)
{-# INLINE _2 #-}

lens :: Functor f => (s -> a) -> (s -> b -> t) -> LensLike f s t a b
lens get set f s = fmap (set s) (f (get s))
{-# INLINE lens #-}
