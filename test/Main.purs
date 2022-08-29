module Test.Main where

import Prelude

import Control.Comonad.Cofree (head, (:<))
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Yoga.Tree (Tree, leaf, scanTree, showTree)
import Yoga.Tree.Zipper (down, findDownWhere, findFromRoot, findUp, flattenLocDepthFirst, fromTree, modifyValue, next, toTree, value)

newtype SpecTree a = SpecTree (Tree a)

instance Show a => Show (SpecTree a) where
  show (SpecTree t) = showTree t

derive instance (Eq a) => Eq (SpecTree a)

sampleTree :: Tree Int
sampleTree =
  1 :<
    [ leaf 2
    , leaf 3
    , 4 :<
        [ leaf 5
        , 6 :< [ leaf 7 ]
        , leaf 8
        ]

    ]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Tree" do

    it "mkTree" do
      let t = 10 :< []
      shouldEqual (head t) 10

    it "Functor" do
      let
        result = 2 :<
          [ leaf 3
          , leaf 4
          , 5 :<
              [ leaf 6
              , 7 :< [ leaf 8 ]
              , leaf 9
              ]
          ]
      SpecTree (((+) 1) <$> sampleTree) `shouldEqual` SpecTree result

    it "scanTree" do
      let
        result =
          1 :<
            [ leaf 3
            , leaf 4
            , 5 :<
                [ leaf 10
                , 11 :< [ leaf 18 ]
                , leaf 13
                ]
            ]
      SpecTree (scanTree (\a b -> a + b) 0 sampleTree) `shouldEqual` SpecTree result

  describe "Zipper" do

    let root = fromTree sampleTree

    it "Modify" do

      let root' = unsafePartial $ toTree $ modifyValue (\a -> 2 * a) (fromJust $ down root)
      let root'' = unsafePartial $ toTree $ modifyValue (\a -> 2 * a) (fromJust $ down >=> next >=> next >=> down $ root)

      let
        result =
          1 :<
            [ leaf 4
            , leaf 3
            , 4 :<
                [ leaf 5
                , 6 :< [ leaf 7 ]
                , leaf 8
                ]

            ]

      let
        result' =
          1 :<
            [ 2 :< []
            , 3 :< []
            , 4 :<
                [ 10 :< []
                , 6 :< [ 7 :< [] ]
                , 8 :< []
                ]
            ]

      SpecTree root' `shouldEqual` SpecTree result
      SpecTree root'' `shouldEqual` SpecTree result'

  -- it "Insert" do

  --   let root1 = unsafePartial $ toTree $ insertAfter (mkLeaf 100) (fromJust $ down root)
  --   let root2 = unsafePartial $ toTree $ insertAfter (mkLeaf 100) (fromJust $ (down root) >>= next >>= next >>= down >>= next >>= down)
  --   let root3 = unsafePartial $ toTree $ insertAfter (mkLeaf 100) (fromJust $ (firstChild root))
  --   let root4 = unsafePartial $ toTree $ insertAfter (mkLeaf 100) (fromJust $ (lastChild root))

  --   let result1 = 1 :< []
  --   let result2 = 1 :< []
  --   let result3 = 1 :< []
  --   let result4 = 1 :< []
  --   shouldEqual (eq root1 result1) true
  --   shouldEqual (eq root2 result2) true
  --   shouldEqual (eq root3 result3) true
  --   shouldEqual (eq root4 result4) true

  it "Should findDownWhere with single node" do
    let tree = 1 :< []
    let loc = fromTree tree
    shouldEqual (Just 1) ((findDownWhere (_ == 1) loc) <#> value)

  it "Should findDownWhere with 2 nodes and 2 levels" do
    let tree = 1 :< [ leaf 2 ]
    let loc = fromTree tree
    shouldEqual (Just 2) ((findDownWhere (_ == 2) loc) <#> value)

  it "Should findDownWhere with 3 nodes and 2 levels" do
    let tree = 1 :< [ leaf 2, leaf 3 ]
    let loc = fromTree tree
    shouldEqual (Just 3) ((findDownWhere (_ == 3) loc) <#> value)

  it "Should findDownWhere with 4 nodes and 2 levels" do
    let tree = 1 :< (leaf <$> [ 2, 3, 4 ])
    let loc = fromTree tree
    shouldEqual (Just 4) ((findDownWhere (_ == 4) loc) <#> value)

  it "Should findDownWhere with 5 nodes and 3 levels" do
    let
      tree = 1 :<
        [ leaf 2
        , leaf 3
        , 4 :< [ leaf 5 ]
        ]
    -- log $ showTree tree
    let loc = fromTree tree
    shouldEqual (Just 5) ((findDownWhere (_ == 5) loc) <#> value)

  it "Should findDownWhere with 6 nodes and 3 levels" do
    let
      tree = 1 :<
        [ leaf 2
        , leaf 3
        , 4 :< [ leaf 5, leaf 6 ]
        ]
    -- log $ showTree tree
    let loc = fromTree tree
    shouldEqual (Just 6) ((findDownWhere (_ == 6) loc) <#> value)

  it "Should findDownWhere with 7 nodes and 4 levels" do
    let
      tree = 1 :<
        [ leaf 2
        , leaf 3
        , 4 :< [ leaf 5, 6 :< [ leaf 7 ] ]
        ]
    -- log $ showTree tree
    let loc = fromTree tree
    shouldEqual (Just 7) ((findDownWhere (_ == 7) loc) <#> value)

  it "Should findDownWhere with 8 nodes and 4 levels" do
    let
      tree = 1 :<
        [ leaf 2
        , leaf 3
        , 4 :< [ leaf 5, 6 :< [ leaf 7 ] ]
        , leaf 8
        ]
    -- log $ showTree tree
    let loc = fromTree tree
    shouldEqual (Just 8) ((findDownWhere (_ == 8) loc) <#> value)

  it "Should findDownWhere with 8 nodes and 4 levels with a step back" do
    let
      tree = 1 :<
        [ leaf 2
        , leaf 3
        , 4 :< [ leaf 5, 6 :< [ leaf 7 ] ]
        , leaf 8
        ]
    -- log $ showTree tree
    let loc = fromTree tree
    shouldEqual (Just 7) ((findDownWhere (_ == 7) loc) <#> value)

  it "Should find 7 from the sampleTree" do
    shouldEqual (Just 7) (findDownWhere (_ == 7) (fromTree sampleTree) <#> value)

  it "Should find 8 from the sampleTree (the bottom) and then find 1 (the top) with findUp" do
    let eight = unsafePartial $ fromJust $ findDownWhere (_ == 8) $ fromTree sampleTree
    shouldEqual (Just 1) (findUp 1 eight <#> value)

  it "Should find 8 from the sampleTree (the bottom) but then not find 7 because it would require a downward traversal" do
    let eight = unsafePartial $ fromJust $ findDownWhere (_ == 8) $ fromTree sampleTree
    shouldEqual Nothing (findUp 7 eight <#> value)

  it "Should find 8 from the sampleTree (the bottom) and then find 7 using findFromRoot" do
    let eight = unsafePartial $ fromJust $ findDownWhere (_ == 8) $ fromTree sampleTree
    shouldEqual (Just 7) (findFromRoot 7 eight <#> value)

  it "Should flatten the Tree into a list of locations following a depth first pattern" do
    let flat = map value $ flattenLocDepthFirst $ fromTree sampleTree
    --log $ showTree sampleTree
    --log $ show flat
    shouldEqual flat [1,2,3,4,5,6,7,8]
