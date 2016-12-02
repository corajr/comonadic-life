module Test.Life3Spec where

import Prelude
import Life (ZipperT)
import Life3
import Life3 as Life3
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Control.Monad.Eff.Class (liftEff)
import Data.Identity (Identity(..))
import Data.Array (length)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)
import Test.QuickCheck.Gen (chooseInt, listOf, vectorOf)
import Test.QuickCheck.Laws (A)
import Test.QuickCheck.Laws.Control.Comonad (checkComonad)
import Test.QuickCheck.Laws.Control.Extend (checkExtend)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..), Proxy2(..))

newtype ArbZZ a = ArbZZ (ZipperT (ZipperT (ZipperT Identity)) a)

derive instance newtypeArbZZ :: Newtype (ArbZZ a) _

instance arbArbZZ :: (Arbitrary a) => Arbitrary (ArbZZ a) where
  arbitrary = do
    n <- chooseInt 1 3
    xs <- vectorOf n (vectorOf n (vectorOf n arbitrary))
    case fromFoldable xs of
      Just xs' -> pure $ ArbZZ xs'
      Nothing -> arbitrary

instance coarbArbZZ :: (Coarbitrary a) => Coarbitrary (ArbZZ a) where
  coarbitrary (ArbZZ a) = coarbitrary x >>> coarbitrary xs
    where x = extract a
          xs :: Array (Array (Array a))
          xs = Life3.toUnfoldable a

instance showArbZZ :: (Show a) => Show (ArbZZ a) where
  show (ArbZZ z) = show z

derive newtype instance eqArbZZ :: (Eq a) => Eq (ArbZZ a)
derive newtype instance ordArbZZ :: (Ord a) => Ord (ArbZZ a)

instance functorArbZZ :: Functor ArbZZ where
  map f (ArbZZ z) = ArbZZ (map f z)

instance extendArbZZ :: Extend ArbZZ where
  extend f (ArbZZ z) = ArbZZ (extend (f <<< ArbZZ) z)

instance comonadArbZZ :: Comonad ArbZZ where
  extract (ArbZZ z) = extract z

prxArbZZ = Proxy :: Proxy (ArbZZ A)
prx2arbZZ = Proxy2 :: Proxy2 ArbZZ

checkInvertsZZ a b =
  quickCheck \(ArbZZ (xs :: ZZ Int)) ->
    (b (a xs)) === xs

-- spec :: forall r. Spec r Unit
spec = do
  describe "fromFoldable" do
    it "inverse of toUnfoldable" $
      quickCheck \(ArbZZ z) -> let xs :: Array (Array (Array Boolean))
                                   xs = Life3.toUnfoldable z
                              in Life3.fromFoldable xs === Just z
  describe "ZZ" do
    it "satisfies Eq laws" $
      liftEff $ checkEq prxArbZZ
    it "satisfies Ord laws" $
      liftEff $ checkOrd prxArbZZ
    it "satisfies Functor laws" $
      liftEff $ checkFunctor prx2arbZZ
    it "satisfies comonad laws: extract" $
      liftEff $ checkComonad prx2arbZZ
    -- it "satisfies comonad laws: associative extend" $
      -- liftEff $ checkExtend prx2arbZZ
  describe "directions" do
    describe "zzUp" do
      it "inverts zzDown" $ checkInvertsZZ zzDown zzUp
    describe "zzDown" do
      it "inverts zzUp" $ checkInvertsZZ zzUp zzDown
    describe "zzLeft" do
      it "inverts zzRight" $ checkInvertsZZ zzRight zzLeft
    describe "zzRight" do
      it "inverts zzLeft" $ checkInvertsZZ zzLeft zzRight
    describe "zzIn" do
      it "inverts zzOut" $ checkInvertsZZ zzOut zzIn
    describe "zzOut" do
      it "inverts zzIn" $ checkInvertsZZ zzIn zzOut
  let f3 = [false, false, false]
      one3 = [1, 1, 1]
      zz = mkZZ [[f3, f3, f3], [f3, [false, true, false], f3], [f3, f3, f3]]
  describe "neighborsZZ" do
    it "should check 26 locations" $
      length neighborsZZ `shouldEqual` 26
  describe "aliveNeighborsZZ" do
    it "returns the number of alive neighbors" $
      Life3.toUnfoldable (extend aliveNeighborsZZ zz) `shouldEqual` [[one3, one3, one3], [one3, [1, 0, 1], one3], [one3, one3, one3]]
  describe "evolveZZ" do
    it "advances the game of life" $
      evolveZZ zz `shouldEqual` mkZZ [[f3, f3, f3], [f3, f3, f3], [f3, f3, f3]]
