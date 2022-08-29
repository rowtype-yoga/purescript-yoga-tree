module Test.Example where

import Prelude

import Control.Comonad.Cofree (head, tail, (:<))
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (uncons)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Effect (Effect)
import Effect.Console (log)
import Yoga.Tree (Tree, mkLeaf)
import Yoga.Tree.Zipper (fromTree, toTree, modifyValue, down, next, root)

-- Serves only to make this file runnable
main :: Effect Unit
main = do
  log "rootOnly_alias"
  log $ drawTree' rootOnly_alias
  log "----"
  log "rootWithLeaves"
  log $ drawTree' rootWithLeaves
  log "----"
  log "rootWithBranchesAndLeaves"
  log $ drawTree' rootWithBranchesAndLeaves
  log "----"
  log "exampleZipper"
  log $ maybe "" identity exampleZipper


-- These examples show how to create a Tree
rootOnly_mkCoFree :: Tree String
rootOnly_mkCoFree = mkLeaf "root"

rootOnly_alias :: Tree String
rootOnly_alias = "root" :< [] -- `:<` is the alias for `mkCofree`

rootWithLeaves :: Tree String
rootWithLeaves =
  "root" :<
    []

rootWithBranchesAndLeaves :: Tree String
rootWithBranchesAndLeaves =
  "1" :< []

-- In our code, we will need to iterate through a Tree and do something
-- with each of its contents. Such a function has already been done
-- via `drawTree`, which iterates through a tree's contents
-- and converts it into a String, indenting them according to their respective
-- level:
-- https://github.com/dmbfm/purescript-tree/blob/v1.3.2/src/Yoga/Tree.purs#L21-L21

-- The below example is a reproduction of the above code but differs from
-- it in that the names are more clearly defined to make it easier to see
-- the pattern one will follow.

drawTree' :: Tree String -> String
drawTree' tree =
  let
    root = head tree
    children = tail tree
  in
    tailRec go {level: 1, result: root <> "\n", current: children }
  where
    go ::      { level :: Int, result :: String, current :: Array (Tree String) }
       -> Step { level :: Int, result :: String, current :: Array (Tree String) }  String
    go rec@{level: l, result: s, current } =
      case uncons current of
        Nothing -> Done s
        Just { head: thisTree, tail: remainingTrees } -> do
          let levelRoot = head thisTree
          let levelChildren = tail thisTree
          let content = (power " " l) <> levelRoot <> "\n"
          Loop
              rec { current = remainingTrees
                  , result = rec.result <> content <>
                        (tailRec go { level: l + 1, result: "", current: levelChildren })
                  }


-- Since we'll also be using the Zipper for the Tree (the `Loc` type),
-- we need to know how that works. For a clearer idea of what a 'Zipper' is,
-- see 'Design Patterns/Zipper.md':
exampleZipper :: Maybe String
exampleZipper = do
  let node1 = fromTree rootWithBranchesAndLeaves
  node1_1 <- down node1
  node1_1_1 <- down node1_1
  node1_1_2 <- next node1_1_1
  let node1_1_2a = modifyValue (\s -> s <> " a") node1_1_2
  let rootWithModification = root node1_1_2a
  pure $ drawTree' $ toTree rootWithModification
