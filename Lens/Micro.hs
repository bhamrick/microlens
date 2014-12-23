module Lens.Micro where

import Control.Applicative
import Data.Functor.Identity

type LensLike f s t a b = (a -> f b) -> s -> f t
type LensLike' f s a = LensLike f s s a a

view :: LensLike (Const a) s t a b -> s -> a
view l = getConst . l Const

over :: LensLike Identity s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: LensLike Identity s t a b -> b -> s -> t
set l = over l . const

_1 :: Functor f => LensLike f (a, x) (b, x) a b
_1 f (a, x) = fmap (flip (,) x) (f a)

_2 :: Functor f => LensLike f (x, a) (x, b) a b
_2 f (x, a) = fmap ((,) x) (f a)

lens :: Functor f => (s -> a) -> (s -> b -> t) -> LensLike f s t a b
lens get set f s = fmap (set s) (f (get s))
