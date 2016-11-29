module Life where


-- based on http://blog.emillon.org/posts/2012-10-18-comonadic-life.html
-- and https://github.com/gelisam/conway/blob/master/src/Conway.hs

import Prelude
import Data.Array as Array
import Control.Apply (lift2)
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend, extend)
import Control.MonadZero (guard)
import Data.Array (filter, length, toUnfoldable, (!!), (..))
import Data.Bounded (class Bounded, bottom)
import Data.Foldable (class Foldable)
import Data.Generic (class Generic, gShow)
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String (joinWith, fromCharArray)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable, unfoldr, replicate)
import Partial.Unsafe (unsafeCrashWith)

class Indexable t i where
  getIndex :: forall a. t a -> i -> a

infix 5 getIndex as !

data ArrayZipper a = ArrayZipper { array :: Array a
                                 , index :: Int
                                 }

derive instance eqArrayZipper :: (Eq a) => Eq (ArrayZipper a)
derive instance ordArrayZipper :: (Ord a) => Ord (ArrayZipper a)
derive instance genericArrayZipper :: (Generic a) => Generic (ArrayZipper a)
instance showArrayZipper :: (Show a) => Show (ArrayZipper a) where
  show (ArrayZipper { array, index }) = show array

getOrDie array index = fromMaybe' (\_ -> unsafeCrashWith msg) (array !! index)
  where msg = "Tried to access element " <> show index <> " of " <> show (length array)

wrapI index i array = (n + index + i) `mod` n
  where n = length array

instance indexableArrayZipper :: Indexable ArrayZipper Int where
  getIndex (ArrayZipper {array, index}) i = getOrDie array index'
    where index' = wrapI index i array

instance functorArrayZipper :: Functor ArrayZipper where
    -- map :: forall a b. (a -> b) -> ArrayZipper a -> ArrayZipper b
    map f (ArrayZipper z@{array, index}) = ArrayZipper (z { array = map f array })

instance extendArrayZipper :: Extend ArrayZipper where
    -- extend :: forall b a. (ArrayZipper a -> b) -> ArrayZipper a -> ArrayZipper b
    extend f z@(ArrayZipper z'@{array}) = ArrayZipper (z' { array = array' })
      where array' = map (f <<< flip shift z) (0 .. (length array - 1))

instance comonadZ :: Comonad ArrayZipper where
    -- extract :: forall a. ArrayZipper a -> a
    extract z = z ! 0

shift :: forall a. Int -> ArrayZipper a -> ArrayZipper a
shift i (ArrayZipper z@{ array, index }) = ArrayZipper (z { index = index' })
    where index' = wrapI index i array

newtype ArrayZipperT w a = ArrayZipperT (w (ArrayZipper a))

derive instance newtypeArrayZipperT :: Newtype (ArrayZipperT w a) _

shiftT :: forall a w. (Functor w) => Int -> ArrayZipperT w a -> ArrayZipperT w a
shiftT i = wrap <<< map (shift i) <<< unwrap

instance indexableZipperT :: Indexable (ArrayZipperT ArrayZipper) (Tuple Int Int) where
  getIndex z (Tuple x y) = extract (extract (shift y (unwrap (shiftT x z))))

instance functorArrayZipperT :: (Functor w) => Functor (ArrayZipperT w) where
  map f = wrap <<< (map <<< map) f <<< unwrap

instance extendArrayZipperT :: (Comonad w) => Extend (ArrayZipperT w) where
  extend f = ArrayZipperT <<< extend go <<< unwrap where
    f' = f <<< wrap
    go wz =
      case extract wz of
        ArrayZipper {array: xs, index: i } ->
          let shifted_wzs = map (\j -> map (shift j) wz) (0 .. (length xs - 1))
              ys = map f' shifted_wzs
          in ArrayZipper {array: ys, index: i}

instance comonadArrayZipperT :: (Comonad w) => Comonad (ArrayZipperT w) where
  extract = extract <<< extract <<< unwrap

instance comonadTransArrayZipperT :: ComonadTrans ArrayZipperT where
  lower = map extract <<< unwrap

type Z a = ArrayZipperT ArrayZipper a

derive instance eqZ :: (Eq a) => Eq (ArrayZipperT ArrayZipper a)

instance showZ :: (Show a) => Show (ArrayZipperT ArrayZipper a) where
  show = show <<< unwrap

neighborIndices :: Array (Tuple Int Int)
neighborIndices = do
  x <- (-1) .. 1
  y <- (-1) .. 1
  let xy = Tuple x y
  guard $ xy /= Tuple 0 0
  pure xy

-- | Boundaries are considered dead.
aliveNeighbors :: Z Boolean -> Int
aliveNeighbors z = length <<< filter id $ map fetch neighborIndices
  where fetch xy = z ! xy

rule :: Z Boolean -> Boolean
rule z =
  case aliveNeighbors z of
    2 -> extract z
    3 -> true
    _ -> false

evolve :: Z Boolean -> Z Boolean
evolve = extend rule

glider :: Z Boolean
glider = rs
  where rs = fromArrays ( replicate 3 fl <>
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
        z' = toArrays z
        f :: Boolean -> Char
        f x = if x then '#' else ' '

toUnfoldableZ :: forall a f. (Unfoldable f) => ArrayZipper a -> f a
toUnfoldableZ (ArrayZipper {array}) = Array.toUnfoldable array

fromArray :: forall a. Array a -> ArrayZipper a
fromArray array = ArrayZipper { array, index: 0}

toArray :: forall a. ArrayZipper a -> Array a
toArray (ArrayZipper {array}) = array

toArrays :: forall a. Z a -> Array (Array a)
toArrays xs = map toArray (toArray (unwrap xs))

fromArrays :: forall a. Array (Array a) -> Z a
fromArrays xs = wrap (map fromArray (fromArray xs))

toUnfoldable :: forall a f. (Unfoldable f) => Z a -> f (f a)
toUnfoldable = Array.toUnfoldable <<< map Array.toUnfoldable <<< toArrays

fromFoldable :: forall a f. (Functor f, Foldable f) => f (f a) -> Z a
fromFoldable x = fromArrays (Array.fromFoldable (map Array.fromFoldable x))
