module Life where

-- based on http://blog.emillon.org/posts/2012-10-18-comonadic-life.html

import Prelude
import Data.Array as Array
import Data.List.NonEmpty as NE
import Control.Applicative (class Applicative)
import Control.Apply (class Apply, lift2)
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans, lower)
import Control.Extend (class Extend, extend)
import Data.Array (filter, length)
import Data.Bounded (class Bounded, bottom)
import Data.Foldable (class Foldable, foldr)
import Data.Identity (Identity(..))
import Data.List (List(..), snoc, last, init, (..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty ((:|))
import Data.String (joinWith, fromCharArray)
import Data.Traversable (class Traversable, traverse)
import Data.Unfoldable (class Unfoldable, replicate)

class (Functor t) <= Shiftable t where
  up :: forall a. t a -> t a
  down :: forall a. t a -> t a

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
  -- extend f (ZipperT w) = ZipperT (extend (extend (f <<< ZipperT) <<< ?rs) w)
  extend f (ZipperT w) = ZipperT (extend (map (f <<< ZipperT) <<< rotations) w)

rotations :: forall w a. (Functor w, Comonad w) => w (NonEmptyList a) -> NonEmptyList (w (NonEmptyList a))
rotations wz = wrap (wz :| map (\f -> map f wz) fs)
  where xs = extract wz
        fs = map (\i -> foldr (<<<) id (repeatUp i)) n_range
        repeatUp :: Int -> Array (NonEmptyList a -> NonEmptyList a)
        repeatUp i = replicate i up
        n = NE.length xs
        n_range = if n > 1 then (1 .. (n - 1)) else Nil

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

instance shiftableZipperT :: Functor w => Shiftable (ZipperT w) where
  up (ZipperT w) = ZipperT (map up w)
  down (ZipperT w) = ZipperT (map down w)

type Zipper a = ZipperT Identity a

type Z a = ZipperT (ZipperT Identity) a

zUp :: forall a. Z a -> Z a
zUp (ZipperT z) = ZipperT (up z)

zDown :: forall a. Z a -> Z a
zDown (ZipperT z) = ZipperT (down z)

zLeft :: forall a. Z a -> Z a
zLeft z = up z

zRight :: forall a. Z a -> Z a
zRight z = down z

zipper :: forall a. a -> ZipperT Identity a
zipper = wrap <<< wrap <<< NE.singleton

emptyZ :: forall a. (Bounded a) => Z a
emptyZ = ZipperT (zipper (NE.singleton bottom))

-- | Neighbors in all 8 directions
neighbors :: forall a. Array (Z a -> Z a)
neighbors = horiz <> vert <> lift2 (>>>) horiz vert
  where horiz = [zLeft, zRight]
        vert = [zUp, zDown]

-- | Boundaries are considered dead.
aliveNeighbors :: Z Boolean -> Int
aliveNeighbors z = length <<< filter id $ map (\x -> extract (x z)) neighbors

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
disp z = joinWith "\n" (map (fromCharArray <<< map f) z')
  where z' :: Array (Array Boolean)
        z' = toUnfoldable z
        f :: Boolean -> Char
        f x = if x then '#' else ' '

toUnfoldableZ :: forall a f. (Unfoldable f) => ZipperT Identity a -> f a
toUnfoldableZ (ZipperT (Identity xs)) = NE.toUnfoldable xs

fromFoldableZ :: forall a f. (Foldable f) => f a -> Maybe (ZipperT Identity a)
fromFoldableZ xs = ZipperT <$> (Identity <$> NE.fromFoldable xs)

toUnfoldable :: forall a f. (Functor f, Unfoldable f) => Z a -> f (f a)
toUnfoldable (ZipperT x) = toUnfoldableZ (map NE.toUnfoldable x)

fromFoldable :: forall a f. (Traversable f, Foldable f) => f (f a) -> Maybe (Z a)
fromFoldable x = ZipperT <$> (traverse NE.fromFoldable x >>= fromFoldableZ)

mkZ :: forall a. (Bounded a) => Array (Array a) -> Z a
mkZ = fromMaybe emptyZ <<< fromFoldable
