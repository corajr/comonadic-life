module Test.LifeComonadLaws where

import Prelude

import Jack (Property, Gen, property, forAll, elements, sized, chooseInt, arrayOfN)
import Life

import Control.Comonad (extract)
import Control.Extend (extend)

import Data.Array (mapMaybe)
import Data.Maybe (fromMaybe)

genBool :: Gen Boolean
genBool = elements [true, false]

genZbool :: Gen (Z Boolean)
genZbool = sized \n' -> do
  let n = min n' 8
  xs <- arrayOfN n (arrayOfN n genBool)
  pure (fromMaybe emptyZ (fromFoldable xs))

f :: Z Boolean -> Boolean
f z = false

g :: Z Boolean -> Boolean
g z = true

-- | extend extract == id
prop_extend_extract_is_id :: Property
prop_extend_extract_is_id =
  forAll genZbool \z ->
    property $ extend extract z == z

-- | extract (extend f) = f
prop_extract_extend_f_is_f :: Property
prop_extract_extend_f_is_f =
  forAll genZbool \z ->
    property $ extract (extend f z) == f z

-- | extend f . extend g = extend (f . extend g)
prop_extend_f_extend_g_is_extend__f_extend_g :: Property
prop_extend_f_extend_g_is_extend__f_extend_g =
  forAll genZbool \z ->
    property $ extend f (extend g z) == extend (f <<< extend g) z

