module Test.LifeSpec where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Life
import Life as Life
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.List as List
import Data.List.Zipper

spec :: forall r. Spec r Unit
spec = do
  let z = Zipper Nil 1 (List.fromFoldable [2, 3])
      z' = Zipper (List.fromFoldable [1]) 2 (List.fromFoldable [3])
      z'' = Zipper (List.fromFoldable [2, 1]) 3 Nil
  describe "upOrWrap" do
    it "moves a zipper up or wraps to end" $ do
      upOrWrap z `shouldEqual` z''
      upOrWrap z' `shouldEqual` z
      upOrWrap z'' `shouldEqual` z'
  describe "downOrWrap" do
    it "moves a zipper down or wraps to beginning" $ do
      downOrWrap z `shouldEqual` z'
      downOrWrap z' `shouldEqual` z''
      downOrWrap z'' `shouldEqual` z
  let z1 = Zipper Nil 1 (List.singleton 2)
      z2 = Zipper Nil 3 (List.singleton 4)
      planeZ = Z (Zipper Nil z1 (List.singleton z2))
      zArray = [[1,2], [3,4]]
  describe "fromFoldable" do
    it "converts a Foldable into a Z" do
      Life.fromFoldable zArray `shouldEqual` Just planeZ
  describe "toUnfoldable" do
    it "converts a Z into an Unfoldable" do
      Life.toUnfoldable planeZ `shouldEqual` zArray
