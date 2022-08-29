module Yoga.Tree where

import Prelude
import Control.Comonad.Cofree (Cofree, head, mkCofree, tail, (:<))
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (snoc, uncons)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Traversable (Accum)

-- | A Rose, or multi-way tree, with values of type `a`.
type Tree a =
  Cofree Array a

type Forest a =
  Array (Tree a)

-- | Create a `Tree` from a `Node` value of type `a` and a `Forest` of children.
mkTree ∷ ∀ a. a -> Forest a -> Tree a
mkTree = mkCofree

mkLeaf ∷ ∀ a. a -> Tree a
mkLeaf = flip mkTree []

leaf :: forall a. a -> Tree a
leaf = mkLeaf

-- | Draw a 2D `String` representation of a `Tree String`.
drawTree ∷ Tree String -> String
drawTree t = tailRec go { level: 0, drawn: (head t) <> "\n", current: (tail t) }
  where
  go ∷ { current ∷ Forest String, drawn ∷ String, level ∷ Int } -> Step { current ∷ Forest String, drawn ∷ String, level ∷ Int } String
  go x = case x { current = uncons x.current } of
    { drawn: s, current: Nothing } -> Done s
    { level: l, drawn: s, current: Just { head: c, tail: cs } } ->
      let
        drawn = (power "       " l) <> "|----> " <> (head c) <> "\n"
      in
        Loop { level: l, drawn: s <> drawn <> (tailRec go { level: l + 1, drawn: "", current: (tail c) }), current: cs }

-- | Draw a 2D `String`  representation of a `Tree` composed of `Show`able
-- | elements.
showTree ∷ ∀ a. Show a => Tree a -> String
showTree = drawTree <<< (map show)

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | that have the same parent.
scanTree ∷ ∀ a b. (a -> b -> b) -> b -> Tree a -> Tree b
scanTree f b n =
  let
    fb = f (head n) b
  in
    fb :< (tailRec go { b: fb, current: (tail n), final: [] })
  where
  go ∷ { final ∷ Forest b, current ∷ Forest a, b ∷ b } -> Step { final ∷ Forest b, current ∷ Forest a, b ∷ b } (Forest b)
  go x = case x { current = uncons x.current } of
    { current: Nothing, final } -> Done final
    { b: b', current: Just { head: c, tail: cs }, final } ->
      let
        fb' = f (head c) b'
      in
        Loop { b: b', current: cs, final: snoc final (fb' :< tailRec go { b: fb', current: (tail c), final: [] }) }

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | that have the same parent, and returning a `Tree` of type `c`.
scanTreeAccum ∷ ∀ a b c. (a -> b -> Accum b c) -> b -> Tree a -> Tree c
scanTreeAccum f b n =
  let
    fb = f (head n) b
  in
    fb.value :< (tailRec go { b: fb.accum, current: (tail n), final: [] })
  where
  go ∷ { final ∷ Forest c, current ∷ Forest a, b ∷ b } -> Step { final ∷ Forest c, current ∷ Forest a, b ∷ b } (Forest c)
  go x = case (x { current = uncons x.current }) of
    { current: Nothing, final } -> Done final
    { b: b', current: Just { head: c, tail: cs }, final } ->
      let
        fb' = f (head c) b'
      in
        Loop { b: b', current: cs, final: snoc final (fb'.value :< tailRec go { b: fb'.accum, current: (tail c), final: [] }) }

-- | Set the value of a node.
setNodeValue ∷ ∀ a. a -> Tree a -> Tree a
setNodeValue a n = a :< (tail n)

-- | Modify the value of a node.
modifyNodeValue ∷ ∀ a. (a -> a) -> Tree a -> Tree a
modifyNodeValue f n = f (head n) :< tail n

-- | Append a child to a node.
appendChild ∷ ∀ a. Tree a -> Tree a -> Tree a
appendChild c n = head n :< snoc (tail n) c
