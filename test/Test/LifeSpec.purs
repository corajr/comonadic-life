module Test.LifeSpec where

import Prelude
import Life
import Data.List.NonEmpty as NE
import Life as Life
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Control.Monad.Eff.Class (liftEff)
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty ((:|))
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

newtype ArbZipperT a = ArbZipperT (ZipperT Identity a)

derive instance newtypeArbZipperT :: Newtype (ArbZipperT a) _

instance arbArbZipperT :: (Arbitrary a) => Arbitrary (ArbZipperT a) where
  arbitrary = do
    (NEL xs) <- arbitrary
    pure (ArbZipperT (wrap (wrap xs)))

instance coarbArbZipperT :: (Coarbitrary a) => Coarbitrary (ArbZipperT a) where
  coarbitrary (ArbZipperT (ZipperT (Identity xs))) = coarbitrary (NE.head xs) >>> coarbitrary xs'
    where xs' :: Array a
          xs' = NE.toUnfoldable xs

instance showArbZipperT :: (Show a) => Show (ArbZipperT a) where
  show (ArbZipperT z) = show z

derive instance eqArbZipperT :: (Eq a) => Eq (ArbZipperT a)
derive instance ordArbZipperT :: (Ord a) => Ord (ArbZipperT a)

instance functorArbZipperT :: Functor ArbZipperT where
  map f (ArbZipperT z) = ArbZipperT (map f z)

instance extendArbZipperT :: Extend ArbZipperT where
  extend f (ArbZipperT z) = ArbZipperT (extend (f <<< ArbZipperT) z)

instance comonadArbZipperT :: Comonad ArbZipperT where
  extract (ArbZipperT z) = extract z

newtype ArbZ a = ArbZ (ZipperT (ZipperT Identity) a)

derive instance newtypeArbZ :: Newtype (ArbZ a) _

instance arbArbZ :: (Arbitrary a) => Arbitrary (ArbZ a) where
  arbitrary = do
    n <- chooseInt 1 3
    xs <- vectorOf n (vectorOf n arbitrary)
    case fromFoldable xs of
      Just xs' -> pure $ ArbZ xs'
      Nothing -> arbitrary

instance coarbArbZ :: (Coarbitrary a) => Coarbitrary (ArbZ a) where
  coarbitrary (ArbZ a) = coarbitrary x >>> coarbitrary xs
    where x = extract a
          xs :: Array (Array a)
          xs = Life.toUnfoldable a

instance showArbZ :: (Show a) => Show (ArbZ a) where
  show (ArbZ z) = show z

derive newtype instance eqArbZ :: (Eq a) => Eq (ArbZ a)
derive newtype instance ordArbZ :: (Ord a) => Ord (ArbZ a)

instance functorArbZ :: Functor ArbZ where
  map f (ArbZ z) = ArbZ (map f z)

instance extendArbZ :: Extend ArbZ where
  extend f (ArbZ z) = ArbZ (extend (f <<< ArbZ) z)

instance comonadArbZ :: Comonad ArbZ where
  extract (ArbZ z) = extract z

prxArbZipperT = Proxy :: Proxy (ArbZipperT A)
prxArbZ = Proxy :: Proxy (ArbZ A)

prx2arbZipperT = Proxy2 :: Proxy2 ArbZipperT
prx2arbZ = Proxy2 :: Proxy2 ArbZ

newtype NEL a = NEL (NonEmptyList a)

instance arbitraryNEL :: Arbitrary a => Arbitrary (NEL a) where
  arbitrary = do
    x <- arbitrary
    n <- chooseInt 0 5
    xs <- listOf n arbitrary
    pure (NEL (wrap (x :| xs)))

checkInverts a b =
  quickCheck \(NEL (xs :: NonEmptyList Int)) ->
    (b (a xs)) === xs

checkInvertsZ a b =
  quickCheck \(ArbZ (xs :: Z Int)) ->
    (b (a xs)) === xs

-- spec :: forall r. Spec r Unit
spec = do
  let nel1 = NE.singleton 1
      nel = wrap (1 :| Cons 2 (Cons 3 Nil))
  describe "up" do
    it "moves the list up" do
      up nel1 `shouldEqual` nel1
      NE.toUnfoldable (up nel) `shouldEqual` [2,3,1]
    it "inverts down" $ checkInverts down up
    it "leaves length the same" $
      quickCheck \(NEL xs :: NEL Int) -> NE.length (up xs) === NE.length xs
  describe "down" do
    it "moves the list down" do
      down nel1 `shouldEqual` nel1
      NE.toUnfoldable (down nel) `shouldEqual` [3,1,2]
    it "inverts up" $ checkInverts up down
    it "leaves length the same" $
      quickCheck \(NEL xs :: NEL Int) -> NE.length (down xs) === NE.length xs
  describe "fromFoldable" do
    it "inverse of toUnfoldable" $
      quickCheck \(ArbZ z) -> let xs :: Array (Array Boolean)
                                  xs = Life.toUnfoldable z
                              in Life.fromFoldable xs === Just z
  describe "ZipperT" do
    it "satisfies Eq laws" $
      liftEff $ checkEq prxArbZipperT
    it "satisfies Ord laws" $
      liftEff $ checkOrd prxArbZipperT
    it "satisfies Functor laws" $
      liftEff $ checkFunctor prx2arbZipperT
    it "satisfies comonad laws: extract" $ do
      liftEff $ checkComonad prx2arbZipperT
    it "satisfies comonad laws: extend" $
      liftEff $ checkExtend prx2arbZipperT
  describe "Z" do
    it "satisfies Eq laws" $
      liftEff $ checkEq prxArbZ
    it "satisfies Ord laws" $
      liftEff $ checkOrd prxArbZ
    it "satisfies Functor laws" $
      liftEff $ checkFunctor prx2arbZ
    it "satisfies comonad laws: extract" $
      liftEff $ checkComonad prx2arbZ
    it "satisfies comonad laws: associative extend" $
      liftEff $ checkExtend prx2arbZ
  describe "directions" do
    let z = mkZ [[1,2,3], [4, 5, 6], [7,8,9]]
    describe "zUp" do
      it "moves the Z up" $
        zUp z `shouldEqual` mkZ [[4, 5, 6], [7,8,9], [1,2,3]]
      it "inverts zDown" $ checkInvertsZ zDown zUp
    describe "zDown" do
      it "moves the Z down" $
        zDown z `shouldEqual` mkZ [[7,8,9], [1,2,3], [4, 5, 6]]
      it "inverts zUp" $ checkInvertsZ zUp zDown
    describe "zLeft" do
      it "moves the Z left" $
        zLeft z `shouldEqual` mkZ [[2,3,1], [5,6,4], [8,9,7]]
      it "inverts zRight" $ checkInvertsZ zRight zLeft
    describe "zRight" do
      it "moves the Z right" $
        zRight z `shouldEqual` mkZ [[3,1,2], [6, 4, 5], [9,7,8]]
      it "inverts zLeft" $ checkInvertsZ zLeft zRight

  let z = mkZ [[false, false, false], [false, true, false], [false, false, false]]
  describe "aliveNeighbors" do
    it "returns the number of alive neighbors" $
      Life.toUnfoldable (extend aliveNeighbors z) `shouldEqual` [[1,1,1], [1,0,1], [1,1,1]]
  describe "evolve" do
    it "advances the game of life" $
      evolve z `shouldEqual` mkZ [[false, false, false], [false, false, false], [false, false, false]]
