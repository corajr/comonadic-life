module Life3 where

import Prelude
import Life (ZipperT(..), up, down)
import Life as Life
import Control.Apply (lift2, lift3)
import Control.Comonad (extract)
import Control.Extend (extend)
import Data.Array (filter, length)
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (class Traversable, traverse)
import Data.Unfoldable (class Unfoldable)

type ZZ a = ZipperT (ZipperT (ZipperT Identity)) a

zzUp :: forall a. ZZ a -> ZZ a
zzUp (ZipperT z) = ZipperT (map up z)

zzDown :: forall a. ZZ a -> ZZ a
zzDown (ZipperT z) = ZipperT (map down z)

zzLeft :: forall a. ZZ a -> ZZ a
zzLeft (ZipperT z) = ZipperT (up z)

zzRight :: forall a. ZZ a -> ZZ a
zzRight (ZipperT z) = ZipperT (down z)

zzIn :: forall a. ZZ a -> ZZ a
zzIn z = up z

zzOut :: forall a. ZZ a -> ZZ a
zzOut z = down z

emptyZZ :: forall a. (Bounded a) => ZZ a
emptyZZ = ZipperT (ZipperT (ZipperT (Identity (NE.singleton (NE.singleton (NE.singleton bottom))))))

-- | Neighbors in all 26 directions
neighborsZZ :: forall a. Array (ZZ a -> ZZ a)
neighborsZZ = horiz <> vert <> depth <>
              lift2 (>>>) horiz vert <> lift2 (>>>) vert depth <> lift2 (>>>) horiz depth <>
              lift3 (\x y z -> x >>> y >>> z) horiz vert depth
  where horiz = [zzLeft, zzRight]
        vert = [zzUp, zzDown]
        depth = [zzIn, zzOut]

-- | Boundaries are considered dead.
aliveNeighborsZZ :: ZZ Boolean -> Int
aliveNeighborsZZ z = length <<< filter id $ map (\x -> extract (x z)) neighborsZZ

ruleZZ :: ZZ Boolean -> Boolean
ruleZZ z =
  case aliveNeighborsZZ z of
    4 -> extract z
    5 -> true
    _ -> false

evolveZZ :: ZZ Boolean -> ZZ Boolean
evolveZZ = extend ruleZZ

toUnfoldable :: forall f a. (Unfoldable f, Functor f) => ZZ a -> f (f (f a))
toUnfoldable (ZipperT z) = Life.toUnfoldable (map NE.toUnfoldable z)

fromFoldable :: forall a f. (Traversable f, Foldable f) => f (f (f a)) -> Maybe (ZZ a)
fromFoldable x = ZipperT <$> (traverse (traverse NE.fromFoldable) x >>= Life.fromFoldable)

mkZZ :: forall a. (Bounded a) => Array (Array (Array a)) -> ZZ a
mkZZ = fromMaybe emptyZZ <<< fromFoldable
