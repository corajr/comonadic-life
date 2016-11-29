module Life where


-- based on http://blog.emillon.org/posts/2012-10-18-comonadic-life.html

import Prelude
import Data.List.Zipper
import Test.QuickCheck.Gen
import Data.List as List
import Data.List.Zipper as Zipper
import Control.Apply (lift2)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Array (filter, length)
import Data.Bounded (class Bounded, bottom)
import Data.Foldable (class Foldable)
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, fromCharArray)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable, unfoldr, replicate)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)

data Z a = Z (Zipper (Zipper a))

instance functorZ :: Functor Z where
    -- map :: forall a b. (a -> b) -> Z a -> Z b
    map f (Z z) = Z (map (map f) z)

instance extendZ :: Extend Z where
    -- extend :: forall b a. (Z a -> b) -> Z a -> Z b
    extend f = map f <<< duplicate
        where duplicate z = Z (map horizontal (vertical z))
              horizontal = genericMove zLeft zRight
              vertical = genericMove zUp zDown
              genericMove :: forall a. (Z a -> Maybe (Z a)) -> (Z a -> Maybe (Z a)) -> Z a -> Zipper (Z a)
              genericMove a b z = Zipper (reverse (maybeIterate a z)) z (maybeIterate b z)

instance comonadZ :: Comonad Z where
    -- extract :: forall a. Z a -> a
    extract (Z z) = extract (extract z)

derive instance eqZ :: (Eq a) => Eq (Z a)

-- this instance is invalid and exists only to support boundedZ
instance ordZ :: (Ord a) => Ord (Z a) where
  compare y z = compare y' z'
    where y' :: Array (Array a)
          y' = toUnfoldable y
          z' :: Array (Array a)
          z' = toUnfoldable z

instance boundedZ :: (Bounded a) => Bounded (Z a) where
  bottom = Z (Zipper Nil (Zipper Nil bottom Nil) Nil)
  top =  Z (Zipper Nil (Zipper Nil top Nil) Nil)

instance showZ :: (Show a) => Show (Z a) where
    show (Z z) = show z

instance arbitraryZ :: (Arbitrary a, Bounded a) => Arbitrary (Z a) where
  arbitrary = do
    n <- chooseInt 0 3
    xs <- vectorOf n (vectorOf n (elements bottom [bottom, top]))
    pure (fromMaybe emptyZ (fromFoldable xs))

instance coarbitraryZ :: (Coarbitrary a) => Coarbitrary (Z a) where
  coarbitrary xs = coarbitrary xs'
    where xs' :: Array (Array a)
          xs' = toUnfoldable xs

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
zUp :: forall a. Z a -> Maybe (Z a)
zUp (Z z) = Z <$> up z

-- | Move down (forward in the top-level Zipper).
zDown :: forall a. Z a -> Maybe (Z a)
zDown (Z z) = Z <$> down z

-- | Move left (backward in all nested Zippers).
zLeft :: forall a. Z a -> Maybe (Z a)
zLeft (Z z) = Z <$> traverse up z

-- | Move left (backward in all nested Zippers).
zRight :: forall a. Z a -> Maybe (Z a)
zRight (Z z) = Z <$> traverse down z

maybeIterate :: forall a f. (Unfoldable f) => (a -> Maybe a) -> a -> f a
maybeIterate f = unfoldr (map dup <<< f)
  where dup a = Tuple a a

neighbors :: forall a. Array (Z a -> Maybe (Z a))
neighbors = horiz <> vert <> lift2 (>=>) horiz vert
  where horiz = [zLeft, zRight]
        vert = [zUp, zDown]

emptyZ :: forall a. (Bounded a) => Z a
emptyZ = Z (Zipper Nil (Zipper Nil bottom Nil) Nil)

-- | Boundaries are considered dead.
aliveNeighbors :: Z Boolean -> Int
aliveNeighbors z = length <<< filter id $ map fetch neighbors
  where fetch dir = case dir z of
          Just z' -> extract z'
          Nothing -> false

rule :: Z Boolean -> Boolean
rule z =
  case aliveNeighbors z of
    2 -> extract z
    3 -> true
    _ -> false

evolve :: Z Boolean -> Z Boolean
evolve = extend rule

glider :: Z Boolean
glider = fromMaybe emptyZ rs
  where rs = fromFoldable ( replicate 3 fl <>
                            [ fs <> [f, t, f] <> fs
                            , fs <> [f, f, t] <> fs
                            , fs <> [t, t, t] <> fs
                            ] <> replicate 3 fl)
        t = true
        f = false
        fs = replicate 6 f
        fl = replicate 15 f


disp :: Z Boolean -> String
disp (Z z) = joinWith "\n" (map (fromCharArray <<< map f) z')
  where z' :: Array (Array Boolean)
        z' = toUnfoldable (Z z)
        f :: Boolean -> Char
        f x = if x then '#' else ' '

toUnfoldable :: forall a f. (Unfoldable f) => Z a -> f (f a)
toUnfoldable (Z z) = Zipper.toUnfoldable (map Zipper.toUnfoldable z)

fromList :: forall a. List (List a) -> Maybe (Z a)
fromList x = Z <$> (traverse Zipper.fromFoldable x >>= Zipper.fromFoldable)

fromFoldable :: forall a f. (Foldable f) => f (f a) -> Maybe (Z a)
fromFoldable x = fromList (map List.fromFoldable (List.fromFoldable x))
