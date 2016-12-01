module Life where

-- based on http://blog.emillon.org/posts/2012-10-18-comonadic-life.html

import Prelude
import Data.List.NonEmpty as NE
import Control.Apply (lift2)
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans, lower)
import Control.Extend (class Extend, extend)
import Data.Array (filter, length)
import Data.Array as Array
import Data.Bounded (class Bounded, bottom)
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.List (List(..), snoc, last, init, (..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.NonEmpty ((:|))
import Data.String (joinWith, fromCharArray)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable, unfoldr, replicate)
import Partial.Unsafe (unsafeCrashWith)

class Shiftable t where
  up :: forall a. t a -> t a
  down :: forall a. t a -> t a
  shift :: forall a. Int -> t a -> t a

shiftDefault :: forall t a. (Shiftable t) => Int -> t a -> t a
shiftDefault 0 xs = xs
shiftDefault n xs =
  shiftDefault (n - 1) (up xs)

newtype ZipperT w a = ZipperT (w (NonEmptyList a))

derive instance eqZipperT :: (Eq a, Eq (w (NonEmptyList a))) => Eq (ZipperT w a)
derive instance ordZipperT :: (Ord a, Ord (w (NonEmptyList a))) => Ord (ZipperT w a)
derive instance newtypeZipperT :: Newtype (ZipperT w a) _

instance showZipperT :: (Show a, Show (w (NonEmptyList a))) => Show (ZipperT w a) where
  show (ZipperT wl) = show wl

instance functorZipperT :: (Functor w) => Functor (ZipperT w) where
  map f (ZipperT wf) = ZipperT (map (map f) wf)

instance extendZipperT :: (Comonad w) => Extend (ZipperT w) where
  -- extend :: forall a b. (ZipperT w a -> b) -> ZipperT w a -> ZipperT w b
  -- extend :: forall a b. (w (NonEmptyList a) -> b) -> w (NonEmptyList a) -> w (NonEmptyList b)
  extend f (ZipperT w) = ZipperT (extend go w)
    where f' = f <<< ZipperT
          go wz = map f' (rotations wz)

rotations :: forall w a. (Comonad w) => w (NonEmptyList a) -> NonEmptyList (w (NonEmptyList a))
rotations wz = map (\i -> map (shift i) wz) n_range
  where n_range = wrap (0 :| n_range')
        n_range' = if n > 1 then (1 .. (n - 1)) else Nil
        n = NE.length (extract wz)

instance comonadZipperT :: (Comonad w) => Comonad (ZipperT w) where
  extract (ZipperT w) = extract (extract w)

instance comonadTransZipperT :: ComonadTrans ZipperT where
  lower (ZipperT w) = map extract w

instance shiftableNEL :: Shiftable NonEmptyList where
  up xs = case NE.uncons xs of
    { head, tail } ->
      case tail of
        Nil -> xs
        Cons x xs' -> wrap (x :| (snoc xs' head))
  down xs = case NE.uncons xs of
    { head, tail } ->
      let maybeXs = do x <- last tail
                       xs' <- init tail
                       pure (wrap (x :| (Cons head xs')))
      in fromMaybe xs maybeXs
  shift n xs = shiftDefault n xs

instance shiftableZipper :: Shiftable (ZipperT Identity) where
  up (ZipperT (Identity xs)) = ZipperT (Identity (up xs))
  down (ZipperT (Identity xs)) = ZipperT (Identity (down xs))
  shift n xs = shiftDefault n xs

type Zipper a = ZipperT Identity a

newtype Z a = Z (ZipperT Identity (ZipperT Identity a))

derive instance newtypeZ :: Newtype (Z a) _

derive newtype instance eqZ :: (Eq a) => Eq (Z a)
derive newtype instance ordZ :: (Ord a) => Ord (Z a)

instance showZ :: (Show a) => Show (Z a) where
  show z = show z'
    where z' :: Array (Array a)
          z' = toUnfoldable z

instance functorZ :: Functor Z where
    -- map :: forall a b. (a -> b) -> Z a -> Z b
    map f (Z z) = Z (map (map f) z)

instance extendZ :: Extend Z where
    -- extend :: forall b a. (Z a -> b) -> Z a -> Z b
    extend f (Z z) = Z (extend (extend f') z)
      where f' = f <<< Z

instance comonadZ :: Comonad Z where
    -- extract :: forall a. Z a -> a
    extract (Z z) = extract (extract z)

-- neighbors :: forall a. Array (Z a -> Maybe (Z a))
-- neighbors = horiz <> vert <> lift2 (>=>) horiz vert
--   where horiz = [zLeft, zRight]
--         vert = [zUp, zDown]

zUp :: forall a. Z a -> Z a
zUp (Z z) = Z (up z)

zDown :: forall a. Z a -> Z a
zDown (Z z) = Z (down z)

zLeft :: forall a. Z a -> Z a
zLeft (Z z) = Z (map up z)

zRight :: forall a. Z a -> Z a
zRight (Z z) = Z (map down z)

zipper :: forall a. a -> Zipper a
zipper = wrap <<< wrap <<< NE.singleton

emptyZ :: forall a. (Bounded a) => Z a
emptyZ = Z (zipper (zipper bottom))

-- | Boundaries are considered dead.
aliveNeighbors :: Z Boolean -> Int
aliveNeighbors _ = 0
-- aliveNeighbors z = length <<< filter id $ map fetch neighbors
--   where fetch dir = case dir z of
--           Just z' -> extract z'
--           Nothing -> false

rule :: Z Boolean -> Boolean
rule z =
  case aliveNeighbors z of
    2 -> false --extract z
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
disp z = joinWith "\n" (map (fromCharArray <<< map f) z')
  where z' :: Array (Array Boolean)
        z' = toUnfoldable z
        f :: Boolean -> Char
        f x = if x then '#' else ' '

toUnfoldableT :: forall a f. (Unfoldable f) => ZipperT Identity a -> f a
toUnfoldableT (ZipperT (Identity xs)) = NE.toUnfoldable xs

fromFoldableT :: forall a f. (Foldable f) => f a -> Maybe (ZipperT Identity a)
fromFoldableT xs = ZipperT <$> (Identity <$> NE.fromFoldable xs)

toUnfoldable :: forall a f. (Functor f, Unfoldable f) => Z a -> f (f a)
toUnfoldable (Z z) = map toUnfoldableT (toUnfoldableT z)

fromFoldable :: forall a f. (Traversable f, Foldable f) => f (f a) -> Maybe (Z a)
fromFoldable x = Z <$> (traverse fromFoldableT x >>= fromFoldableT)
