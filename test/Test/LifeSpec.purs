module Test.LifeSpec where

import Prelude
import Life
import Data.List.Zipper
import Data.List as List
import Life as Life
import Control.Comonad (extract)
import Control.Extend (extend)
import Data.Array (length)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

comonadLaws = do
  describe "Z Comonad laws" do
    it "extend extract = id" $
      quickCheck \(xs :: Z Boolean) -> extend extract xs === xs
    it "extract <<< extend f = f" $
      quickCheck \(xs :: Z Boolean) (f :: Z Boolean -> Boolean) -> extract (extend f xs) === f xs
    it "extend f <<< extend g = extend (f <<< extend g)" $
      quickCheck \(xs :: Z Boolean) (f :: Z Boolean -> Boolean) (g :: Z Boolean -> Boolean) -> extend f (extend g xs) === extend (f <<< extend g) xs

checkInverse a b =
  quickCheck \(xs :: Z Boolean) -> (case a xs of
                                       Just xs' -> b xs' === Just xs
                                       Nothing -> xs === xs)

-- spec :: forall r. Spec r Unit
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
      z1' = Zipper (List.singleton 1) 2 Nil
      z2' = Zipper (List.singleton 3) 4 Nil
      planeZ = Z (Zipper Nil z1 (List.singleton z2))
      planeZ' = Z (Zipper (List.singleton z1) z2 Nil)
      planeZr = Z (Zipper Nil z1' (List.singleton z2'))
      planeZr' = Z (Zipper (List.singleton z1') z2' Nil)
      zArray = [[1,2], [3,4]]
      metaPlaneZ = [[planeZ, planeZr], [planeZ', planeZr']]
      metaPlaneZ' = Life.fromFoldable metaPlaneZ
  describe "fromFoldable" do
    it "converts a Foldable into a Z" do
      Life.fromFoldable zArray `shouldEqual` Just planeZ
  describe "toUnfoldable" do
    it "converts a Z into an Unfoldable" do
      Life.toUnfoldable planeZ `shouldEqual` zArray
  describe "zDown" do
    it "moves the cursor down one" do
      zDown planeZ `shouldEqual` Just planeZ'
      zDown planeZr `shouldEqual` Just planeZr'
    it "inverts zUp" $ checkInverse zDown zUp
  describe "zUp" do
    it "moves the cursor up one" do
      zUp planeZ' `shouldEqual` Just planeZ
      zUp planeZr' `shouldEqual` Just planeZr
    it "inverts zDown" $ checkInverse zUp zDown
  describe "zRight" do
    it "moves the cursor right one" do
      zRight planeZ `shouldEqual` Just planeZr
      zRight planeZ' `shouldEqual` Just planeZr'
    it "inverts zLeft" $ checkInverse zLeft zRight
  describe "zLeft" do
    it "moves the cursor left one" do
      zLeft planeZr `shouldEqual` Just planeZ
      zLeft planeZr' `shouldEqual` Just planeZ'
    it "inverts zRight" $ checkInverse zRight zLeft
  describe "maybeIterate" do
    it "iterates but discards the original input" do
      maybeIterate (\x -> if x >= 8 then Nothing else Just (x*2)) 1 `shouldEqual` [2,4,8]
  describe "extend" do
    it "should turn a Z a into a Z (Z a)" do
      Just (extend id planeZ) `shouldEqual` metaPlaneZ'
    it "should return the original plane when combined with extract" do
      extend extract planeZ `shouldEqual` planeZ
  -- comonadLaws
  let sampleArray = [[false, true, false], [false, false, false], [false, false, false]]
      sampleArray' = [[1, 0, 1], [1, 1, 1], [0, 0, 0]]
      sampleBoard = fromMaybe emptyZ (Life.fromFoldable sampleArray)
      sampleBoard' = fromMaybe emptyZ (Life.fromFoldable sampleArray')
  describe "neighbors" do
    it "refers to 8 directions" $
      length neighbors `shouldEqual` 8
  describe "aliveNeighbors" do
    it "returns the number of alive neighbors" do
      extend aliveNeighbors sampleBoard `shouldEqual` sampleBoard'
