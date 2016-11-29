module Test.LifeSpec where

import Prelude
import Life
import Data.List.Zipper
import Data.List as List
import Life as Life
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Array (length, replicate)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

newtype ArbZ a = ArbZ (ArrayZipperT ArrayZipper a)

instance arbArbZ :: (Arbitrary a) => Arbitrary (ArbZ a) where
  arbitrary = do
    n <- chooseInt 1 3
    xs <- vectorOf n (vectorOf n arbitrary)
    pure $ ArbZ (fromArrays xs)

instance coarbArbZ :: (Coarbitrary a) => Coarbitrary (ArbZ a) where
  coarbitrary (ArbZ a) = coarbitrary (toArrays a)

instance showArbZ :: (Show a) => Show (ArbZ a) where
  show (ArbZ z) = show z

derive instance eqArbZ :: (Eq a) => Eq (ArbZ a)

instance functorArbZ :: Functor ArbZ where
  map f (ArbZ z) = ArbZ (map f z)

instance extendArbZ :: Extend ArbZ where
  extend f (ArbZ z) = ArbZ (extend (f <<< ArbZ) z)

instance comonadArbZ :: Comonad ArbZ where
  extract (ArbZ z) = extract z

comonadLaws = do
  describe "Z Comonad laws" do
    it "extend extract = id" $
      quickCheck \(xs :: ArbZ Int) -> extend extract xs === xs
    it "extract <<< extend f = f" $
      quickCheck \(xs :: ArbZ Int) (f :: ArbZ Int -> Int) -> extract (extend f xs) === f xs
    it "extend f <<< extend g = extend (f <<< extend g)" $
      quickCheck \(xs :: ArbZ Int) (f :: ArbZ Int -> Boolean) (g :: ArbZ Int -> Int) -> extend f (extend g xs) === extend (f <<< extend g) xs

-- spec :: forall r. Spec r Unit
spec = do
  describe "fromArrays" do
    it "inverse of toArrays" $
      quickCheck \(ArbZ xs :: ArbZ Int) -> let xs' = Life.toArrays xs
                                           in fromArrays xs' === xs
  comonadLaws
  let sampleArray = [[false, false, false], [false, true, false], [false, false, false]]
      sampleArray' = [[1, 1, 1], [1, 0, 1], [1, 1, 1]]
      sampleBoard = fromArrays sampleArray
      sampleBoard' = fromArrays sampleArray'
  describe "neighborIndices" do
    it "refers to 8 directions" $
      length neighborIndices `shouldEqual` 8
  describe "aliveNeighbors" do
    it "returns the number of alive neighbors" do
      extend aliveNeighbors sampleBoard `shouldEqual` sampleBoard'
