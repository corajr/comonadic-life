module Life where

import Prelude
import Data.List.Zipper
import Data.Maybe (Maybe(..))
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable  (class Unfoldable, unfoldr)

data Z a = Z (Zipper (Zipper a))

instance functorZ :: Functor Z where
    -- map :: forall a b. (a -> b) -> Z a -> Z b
    map f (Z z) = Z (map (map f) z)

instance extendZ :: Extend Z where
    -- extend :: forall b a. (Z a -> b) -> Z a -> Z b
    extend f (Z z) = ?rhs --Z <$> go f up <*> f <*> go f down
        -- where go f d = (<$>) f <<< maybeIterate d

instance comonadZ :: Comonad Z where
    -- extract :: forall a. Z a -> a
    extract (Z z) = extract (extract z)

-- | Moves cursor toward beginning, or wraps back around to end.
upOrWrap :: forall a. Zipper a -> Zipper a
upOrWrap z = case up z of
  Just z' -> z'
  Nothing -> end z

-- | Moves cursor toward end, or wraps back around to beginning.
downOrWrap :: forall a. Zipper a -> Zipper a
downOrWrap z = case down z of
  Just z' -> z'
  Nothing -> beginning z

-- | Move up (backward in the top-level Zipper).
zUp :: forall a. Z a -> Z a
zUp (Z z) = Z (upOrWrap z)

-- | Move down (forward in the top-level Zipper).
zDown :: forall a. Z a -> Z a
zDown (Z z) = Z (downOrWrap z)

-- | Move left (backward in all nested Zippers).
zLeft :: forall a. Z a -> Z a
zLeft (Z z) = Z (map upOrWrap z)

-- | Move left (backward in all nested Zippers).
zRight :: forall a. Z a -> Z a
zRight (Z z) = Z (map downOrWrap z)

maybeIterate :: forall a f. (Unfoldable f) => (a -> Maybe a) -> a -> f a
maybeIterate f = unfoldr (map dup <<< f)
  where dup a = Tuple a a
