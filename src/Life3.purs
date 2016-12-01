module Life3 where

import Prelude
import Life
import Life as Life
import Control.Apply (lift2, lift3)
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans, lower)
import Control.Extend (class Extend, extend)
import Data.Array (filter, length)
import Data.Foldable (class Foldable)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable, traverse)
import Data.Unfoldable (class Unfoldable, replicate)

newtype ZZ a = ZZ (ZipperT Identity (ZipperT Identity (ZipperT Identity a)))

derive instance newtypeZZ :: Newtype (ZZ a) _
derive newtype instance eqZZ :: (Eq a) => Eq (ZZ a)
derive newtype instance ordZZ :: (Ord a) => Ord (ZZ a)

instance showZZ :: (Show a) => Show (ZZ a) where
  show z = show z'
    where z' :: Array (Array (Array a))
          z' = toUnfoldable z

instance functorZZ :: Functor ZZ where
    map f (ZZ z) = ZZ (map ((map <<< map) f) z)

instance extendZZ :: Extend ZZ where
  extend f (ZZ w) = ZZ (extend (map (f <<< ZZ) <<< rotations) w)

instance comonadZZ :: Comonad ZZ where
    extract (ZZ z) = extract (extract (extract z))

zzUp (ZZ z) = ZZ (up z)
zzDown (ZZ z) = ZZ (down z)
zzLeft (ZZ z) = ZZ (map up z)
zzRight (ZZ z) = ZZ (map down z)
zzIn (ZZ z) = ZZ (map (map up) z)
zzOut (ZZ z) = ZZ (map (map down) z)

emptyZZ :: forall a. (Bounded a) => ZZ a
emptyZZ = ZZ (zipper (zipper (zipper bottom)))

-- | Neighbors in all 8 directions
neighbors :: forall a. Array (ZZ a -> ZZ a)
neighbors = horiz <> vert <> lift2 (>>>) horiz vert <> lift3 (\x y z -> x >>> y >>> z) horiz vert depth
  where horiz = [zzLeft, zzRight]
        vert = [zzUp, zzDown]
        depth = [zzIn, zzOut]

-- | Boundaries are considered dead.
aliveNeighbors :: Z Boolean -> Int
aliveNeighbors z = length <<< filter id $ map (\x -> extract (x z)) neighbors

rule :: Z Boolean -> Boolean
rule z =
  case aliveNeighbors z of
    2 -> extract z
    3 -> true
    _ -> false

evolve :: ZZ Boolean -> ZZ Boolean
evolve = extend rule

toUnfoldable :: forall f a. (Unfoldable f, Functor f) => ZZ a -> f (f (f a))
toUnfoldable (ZZ z) = map (Life.toUnfoldable <<< Z) (toUnfoldableT z)

fromFoldable :: forall a f. (Traversable f, Foldable f) => f (f (f a)) -> Maybe (ZZ a)
fromFoldable x = ZZ <$> (traverse fromFoldable' x >>= fromFoldableT)
  where fromFoldable' = map (\(Z z) -> z) <<< Life.fromFoldable

mkZZ :: forall a. (Bounded a) => Array (Array a) -> Z a
mkZZ = fromMaybe emptyZZ <<< fromFoldable

