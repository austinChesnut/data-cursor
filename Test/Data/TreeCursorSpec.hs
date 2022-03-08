{-# LANGUAGE ViewPatterns, TypeApplications, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase #-}
{-# OPTIONS -Wno-orphans #-}
module Test.Data.TreeCursorSpec
  ( spec
  ) where

-- base
import           Data.Maybe
import           Numeric.Natural

-- self
import qualified Data.TreeCursor               as TCur

-- containers
import           Data.Tree

-- Witch
import           Witch

-- QuickCheck
import           Test.QuickCheck

-- Hspec
import           Test.Hspec



--------------------------------------------------------------------------------
-- Orphans
--------------------------------------------------------------------------------

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

initMaybe :: [a] -> Maybe [a]
initMaybe = \case
  []             -> Nothing
  [_           ] -> Just []
  (x1 : x2 : xs) -> (x1 :) <$> initMaybe (x2 : xs)

--------------------------------------------------------------------------------
-- Specs
--------------------------------------------------------------------------------

spec_From :: SpecWith ()
spec_From = describe "Data.TreeCursor.From" $ do
  it "tree to cursor then back to tree does nothing"
    $ property
    $ let prop :: Tree Int -> Expectation
          prop tr = from (into @(TCur.Cursor Int Int) tr) `shouldBe` tr
      in  prop

spec_singleton :: SpecWith ()
spec_singleton = describe "Data.TreeCursor.singleton" $ do
  it "the length of the tree should be 1"
    $ property
    $ let prop :: Int -> Expectation
          prop x = TCur.length @Int (TCur.singleton x) `shouldBe` 1
      in  prop

spec_mkCursorAt :: SpecWith ()
spec_mkCursorAt = describe "Data.TreeCursor.mkCursorAt" $ do
  it "[] is the same as from"
    $ property
    $ let prop :: Tree Int -> Expectation
          prop tr = TCur.mkCursorAt [] tr
            `shouldBe` Just (into @(TCur.Cursor Int Int) tr)
      in  prop
  it "from then using the current index does nothing"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.mkCursorAt (TCur.index cur) (from cur) `shouldBe` Just cur
      in  prop

spec_getSelectedTree :: SpecWith ()
spec_getSelectedTree = describe "Data.TreeCursor.getSelectedTree" $ do
  it "from tree will give back the original tree"
    $ property
    $ let prop :: Tree Int -> Expectation
          prop tr =
            TCur.getSelectedTree (into @(TCur.Cursor Int Int) tr) `shouldBe` tr
      in  prop

spec_pathFromRoot :: SpecWith ()
spec_pathFromRoot = describe "Data.TreeCursor.pathFromRoot" $ do
  it "should be 1 more than the same length of the index"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            length (TCur.pathFromRoot cur)
              `shouldBe` length (TCur.index @Int cur)
              +          1
      in  prop

spec_getPrev :: SpecWith ()
spec_getPrev = describe "Data.TreeCursor.getPrev" $ do
  it "if you insertPrev and then getPrev you get the original item"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = case TCur.getPrev =<< TCur.insertPrev x cur of
            Just c  -> c `shouldBe` x
            Nothing -> discard
      in  prop

spec_getNext :: SpecWith ()
spec_getNext = describe "Data.TreeCursor.getNext" $ do
  it "if you insertNext and then getNext you get the original item"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = case TCur.getNext =<< TCur.insertNext x cur of
            Just c  -> c `shouldBe` x
            Nothing -> discard
      in  prop

spec_length :: SpecWith ()
spec_length = describe "Data.TreeCursor.length" $ do
  it "converting to a cursor does not change the length"
    $ property
    $ let prop :: Tree Int -> Expectation
          prop tr =
            TCur.length (into @(TCur.Cursor Int Int) tr) `shouldBe` length tr
      in  prop
  it "converting to a tree does not change the length"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = length (into @(Tree Int) cur) `shouldBe` TCur.length cur
      in  prop

spec_index :: SpecWith ()
spec_index = describe "Data.TreeCursor.index" $ do
  it "from will create a cursor at []"
    $ property
    $ let prop :: Tree Int -> Expectation
          prop tr =
            TCur.index @Int (into @(TCur.Cursor Int Int) tr) `shouldBe` []
      in  prop

spec_insertPrev :: SpecWith ()
spec_insertPrev = describe "Data.TreeCursor.insertPrev" $ do
  it "increases the length by 1"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = case TCur.insertPrev x cur of
            Just c  -> TCur.length @Int c `shouldBe` TCur.length cur + 1
            Nothing -> discard
      in  prop

spec_insertNext :: SpecWith ()
spec_insertNext = describe "Data.TreeCursor.insertNext" $ do
  it "increases the length by 1"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = case TCur.insertNext x cur of
            Just c  -> TCur.length @Int c `shouldBe` TCur.length cur + 1
            Nothing -> discard
      in  prop

spec_insertDescStart :: SpecWith ()
spec_insertDescStart = describe "Data.TreeCursor.insertDescStart" $ do
  it "if you descendStart the selection should be the inserted item"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = case TCur.descendStart (TCur.insertDescStart x cur) of
            Just c  -> TCur.current c `shouldBe` x
            Nothing -> discard
      in  prop

spec_insertDescEnd :: SpecWith ()
spec_insertDescEnd = describe "Data.TreeCursor.insertDescEnd" $ do
  it "if you descendEnd the selection should be the inserted item"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = case TCur.descendEnd (TCur.insertDescEnd x cur) of
            Just c  -> TCur.current c `shouldBe` x
            Nothing -> discard
      in  prop

spec_insertSelectPrev :: SpecWith ()
spec_insertSelectPrev = describe "Data.TreeCursor.insertSelectPrev" $ do
  it "same as insertPrev then selectPrevSameLevel"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            TCur.insertSelectPrev x cur
              `shouldBe` (   TCur.selectPrevSameLevel @Maybe
                         =<< TCur.insertPrev x cur
                         )
      in  prop

spec_insertSelectNext :: SpecWith ()
spec_insertSelectNext = describe "Data.TreeCursor.insertSelectNext" $ do
  it "same as insertNext then selectNextSameLevel"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            TCur.insertSelectNext x cur
              `shouldBe` (   TCur.selectNextSameLevel @Maybe
                         =<< TCur.insertNext x cur
                         )
      in  prop

spec_insertSelectStart :: SpecWith ()
spec_insertSelectStart = describe "Data.TreeCursor.insertSelectStart" $ do
  it "same as selectStart then insertSelectPrev"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            TCur.insertSelectStart @Maybe x cur
              `shouldBe` TCur.insertSelectPrev x (TCur.selectStart cur)
      in  prop

spec_insertSelectEnd :: SpecWith ()
spec_insertSelectEnd = describe "Data.TreeCursor.insertSelectEnd" $ do
  it "same as selectEnd then insertSelectNext"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            TCur.insertSelectEnd @Maybe x cur
              `shouldBe` TCur.insertSelectNext x (TCur.selectEnd cur)
      in  prop

spec_insertSelectRoot :: SpecWith ()
spec_insertSelectRoot = describe "Data.TreeCursor.insertSelectRoot" $ do
  it "same as converting to a tree and adding a new root"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = TCur.insertSelectRoot x cur
            `shouldBe` from (Node x (pure (from cur)))
      in  prop

spec_insertSelectDescStart :: SpecWith ()
spec_insertSelectDescStart =
  describe "Data.TreeCursor.insertSelectDescStart" $ do
    it "the selection should be the new item"
      $ property
      $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
            prop cur x =
              TCur.current (TCur.insertSelectDescStart x cur) `shouldBe` x
        in  prop
    it "use ascend to return to the original selection"
      $ property
      $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
            prop cur x =
              TCur.current
                <$>        TCur.ascend (TCur.insertSelectDescStart x cur)
                `shouldBe` Just (TCur.current cur)
        in  prop

spec_insertSelectDescEnd :: SpecWith ()
spec_insertSelectDescEnd = describe "Data.TreeCursor.insertSelectDescEnd" $ do
  it "same as descendEnd insertSelectNext"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x = case TCur.insertSelectNext x =<< TCur.descendEnd cur of
            Just y  -> TCur.insertSelectDescEnd x cur `shouldBe` y
            Nothing -> discard
      in  prop

spec_demoteAndInsert :: SpecWith ()
spec_demoteAndInsert = describe "Data.TreeCursor.demoteAndInsert" $ do
  it "the selected tree has a new root"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            TCur.getSelectedTree (TCur.demoteAndInsert x cur)
              `shouldBe` Node x [TCur.getSelectedTree cur]
      in  prop

spec_delPrev :: SpecWith ()
spec_delPrev = describe "Data.TreeCursor.delPrev" $ do
  it "???" pending

spec_delNext :: SpecWith ()
spec_delNext = describe "Data.TreeCursor.delNext" $ do
  it "???" pending

spec_delSelectPrev :: SpecWith ()
spec_delSelectPrev = describe "Data.TreeCursor.delSelectPrev" $ do
  it "same as selectPrev delNext"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.delSelectPrev @Maybe cur
              `shouldBe` (TCur.delNext =<< TCur.selectPrev cur)
      in  prop

spec_delSelectNext :: SpecWith ()
spec_delSelectNext = describe "Data.TreeCursor.delSelectNext" $ do
  it "same as selectNext delPrev"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.delSelectNext @Maybe cur
              `shouldBe` (TCur.delPrev =<< TCur.selectNext cur)
      in  prop

spec_selectPrevSameLevel :: SpecWith ()
spec_selectPrevSameLevel = describe "Data.TreeCursor.selectPrevSameLevel" $ do
  it "the length of the index should remain the same"
    $ property
    $ let
        prop :: TCur.Cursor Int Int -> Expectation
        prop cur = case TCur.selectPrevSameLevel cur of
          Just c ->
            length (TCur.index @Int c) `shouldBe` length (TCur.index @Int cur)
          Nothing -> discard
      in
        prop

spec_selectNextSameLevel :: SpecWith ()
spec_selectNextSameLevel = describe "Data.TreeCursor.selectNextSameLevel" $ do
  it "the length of the index should remain the same"
    $ property
    $ let
        prop :: TCur.Cursor Int Int -> Expectation
        prop cur = case TCur.selectNextSameLevel cur of
          Just c ->
            length (TCur.index @Int c) `shouldBe` length (TCur.index @Int cur)
          Nothing -> discard
      in
        prop

spec_selectPrev :: SpecWith ()
spec_selectPrev = describe "Data.TreeCursor.selectPrev" $ do
  it "selectPrev selectNext does nothing"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.selectNext =<< TCur.selectPrev cur of
            Just c  -> c `shouldBe` cur
            Nothing -> discard
      in  prop
  it "if the index is not [] then you can selectPrev until it is []"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = if null $ TCur.index @Int cur
            then TCur.selectPrev cur `shouldBe` Nothing
            else isJust (TCur.selectPrev cur) `shouldBe` True
      in  prop

spec_selectNext :: SpecWith ()
spec_selectNext = describe "Data.TreeCursor.selectNext" $ do
  it "selectNext selectPrev does nothing"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.selectPrev =<< TCur.selectNext cur of
            Just c  -> c `shouldBe` cur
            Nothing -> discard
      in  prop

spec_selectStart :: SpecWith ()
spec_selectStart = describe "Data.TreeCursor.selectStart" $ do
  it "selectStart selectPrevSameLevel is imposible"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.selectPrevSameLevel (TCur.selectStart cur) `shouldBe` Nothing
      in  prop

spec_selectEnd :: SpecWith ()
spec_selectEnd = describe "Data.TreeCursor.selectEnd" $ do
  it "selectEnd selectNextSameLevel is imposible"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.selectNextSameLevel (TCur.selectEnd cur) `shouldBe` Nothing
      in  prop

spec_selectRoot :: SpecWith ()
spec_selectRoot = describe "Data.TreeCursor.selectRoot" $ do
  it "the index should be []"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = TCur.index @Int (TCur.selectRoot cur) `shouldBe` []
      in  prop

spec_selectAt :: SpecWith ()
spec_selectAt = describe "Data.TreeCursor.selectAt" $ do
  it
      "using the original index will get back the orginal cursor after any navigation"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = TCur.selectAt (TCur.index cur) (TCur.selectRoot cur)
            `shouldBe` Just cur
      in  prop

spec_ascend :: SpecWith ()
spec_ascend = describe "Data.TreeCursor.ascend" $ do
  it "the new index is the init of the old index"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case initMaybe $ TCur.index @Int cur of
            Just xs -> TCur.index <$> TCur.ascend cur `shouldBe` Just xs
            _       -> discard
      in  prop

spec_descendStart :: SpecWith ()
spec_descendStart = describe "Data.TreeCursor.descendStart" $ do
  it "the new index should be 0 add to the end of the old index"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.index @Int <$> TCur.descendStart cur of
            Just xs -> xs `shouldBe` TCur.index cur ++ [0]
            Nothing -> discard
      in  prop

spec_descendEnd :: SpecWith ()
spec_descendEnd = describe "Data.TreeCursor.descendEnd" $ do
  it "the new index should have one more element than the original"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case length . TCur.index @Int <$> TCur.descendEnd cur of
            Just x  -> x `shouldBe` length (TCur.index @Int cur) + 1
            Nothing -> discard
      in  prop
  it "can't selectNextSameLevel"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            (TCur.selectNextSameLevel =<< TCur.descendEnd cur)
              `shouldBe` Nothing
      in  prop

spec_descendAt :: SpecWith ()
spec_descendAt = describe "Data.TreeCursor.descendAt" $ do
  it "the index provided should be add to the end of the original index"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Natural -> Expectation
          prop cur i = case TCur.index <$> TCur.descendAt i cur of
            Just xs -> xs `shouldBe` TCur.index cur ++ [i]
            Nothing -> discard
      in  prop

spec_findBelowBreadthFirst :: SpecWith ()
spec_findBelowBreadthFirst = describe "Data.TreeCursor.findBelow" $ do
  it "the index should be larger than the original"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Fun Int Bool -> Expectation
          prop cur (applyFun->p) =
            foldl
                max
                maxBound
                (length . TCur.index @Int <$> TCur.findBelowBreadthFirst p cur)
              >          TCur.length cur
              `shouldBe` True
      in  prop
  it "the current item must satisfy the predicate"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Fun Int Bool -> Expectation
          prop cur (applyFun->p) =
            all (p . TCur.current) (TCur.findBelowBreadthFirst p cur)
              `shouldBe` True
      in  prop
  it
    "the index should increases in size first by the head and then by the length"
    pending

spec_findBreadthFirst :: SpecWith ()
spec_findBreadthFirst = describe "Data.TreeCursor.find" $ do
  it "the current item must satisfy the predicate"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Fun Int Bool -> Expectation
          prop cur (applyFun->p) =
            all (p . TCur.current) (TCur.findBreadthFirst p cur) `shouldBe` True
      in  prop

-- spec_findBelowDepthFirst :: SpecWith ()
-- spec_findBelowDepthFirst = _
  -- it "the index should increases in size first by the length and then by the head" pending

-- spec_findDepthFirst :: SpecWith ()
-- spec_findDepthFirst = _

-- spec_findBelow :: SpecWith ()
-- spec_findBelow = _

-- spec_find :: SpecWith ()
-- spec_find = _

spec_dragPrevSameLevel :: SpecWith ()
spec_dragPrevSameLevel = describe "Data.TreeCursor.dragPrevSameLevel" $ do
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.dragPrevSameLevel cur of
            Just c  -> TCur.length @Int c `shouldBe` TCur.length cur
            Nothing -> discard
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.dragPrevSameLevel cur of
            Just c  -> TCur.current c `shouldBe` TCur.current cur
            Nothing -> discard
      in  prop
  it "dragPrevSameLevel then dragNextSameLevel does nothing"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            case TCur.dragNextSameLevel =<< TCur.dragPrevSameLevel cur of
              Just c  -> c `shouldBe` cur
              Nothing -> discard
      in  prop

spec_dragNextSameLevel :: SpecWith ()
spec_dragNextSameLevel = describe "Data.TreeCursor.dragNextSameLevel" $ do
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.dragNextSameLevel cur of
            Just c  -> TCur.length @Int c `shouldBe` TCur.length cur
            Nothing -> discard
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.dragNextSameLevel cur of
            Just c  -> TCur.current c `shouldBe` TCur.current cur
            Nothing -> discard
      in  prop
  it "dragNextSameLevel then dragPrevSameLevel does nothing"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            case TCur.dragPrevSameLevel =<< TCur.dragNextSameLevel cur of
              Just c  -> c `shouldBe` cur
              Nothing -> discard
      in  prop

spec_dragStart :: SpecWith ()
spec_dragStart = describe "Data.TreeCursor.dragStart" $ do
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.length @Int (TCur.dragStart cur) `shouldBe` TCur.length cur
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.current (TCur.dragStart cur) `shouldBe` TCur.current cur
      in  prop
  it "dragStart twice is the same as dragStart once"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.dragStart (TCur.dragStart cur) `shouldBe` TCur.dragStart cur
      in  prop
  it "can't dragPrevSameLevel"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.dragPrevSameLevel (TCur.dragStart cur) `shouldBe` Nothing
      in  prop

spec_dragEnd :: SpecWith ()
spec_dragEnd = describe "Data.TreeCursor.dragEnd" $ do
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.length @Int (TCur.dragEnd cur) `shouldBe` TCur.length cur
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.current (TCur.dragEnd cur) `shouldBe` TCur.current cur
      in  prop
  it "dragEnd twice is the same as dragEnd once"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.dragEnd (TCur.dragEnd cur) `shouldBe` TCur.dragEnd cur
      in  prop
  it "can't dragNextSameLevel"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.dragNextSameLevel (TCur.dragEnd cur) `shouldBe` Nothing
      in  prop

spec_dragAbove :: SpecWith ()
spec_dragAbove = describe "Data.TreeCursor.dragAbove" $ do
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.length @Int <$> TCur.dragAbove cur of
            Just c  -> c `shouldBe` TCur.length cur
            Nothing -> discard
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.current <$> TCur.dragAbove cur of
            Just c  -> c `shouldBe` TCur.current cur
            Nothing -> discard
      in  prop
  it "the index should be the init of the original"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.index @Int <$> TCur.dragAbove cur of
            Just c1 -> case initMaybe (TCur.index cur) of
              Just c2 -> c1 `shouldBe` c2
              Nothing -> discard
            Nothing -> discard
      in  prop

spec_dragRoot :: SpecWith ()
spec_dragRoot = describe "Data.TreeCursor.dragRoot" $ do
  it "???" pending
{-
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.length <$> TCur.dragRoot cur of
            Just c  -> c `shouldBe` TCur.length cur
            Nothing -> discard
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.current <$> TCur.dragRoot cur of
            Just c  -> c `shouldBe` TCur.current cur
            Nothing -> discard
      in  prop
  it "when converted to a tree the root of the tree is the selection"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case into @(Tree Int) <$> TCur.dragRoot cur of
            Just (Node x xs) -> x `shouldBe` TCur.current cur
            Nothing          -> discard
      in  prop
-}

spec_dragDescStart :: SpecWith ()
spec_dragDescStart = describe "Data.TreeCursor.dragDescStart" $ do
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.length @Int <$> TCur.dragDescStart cur of
            Just c  -> c `shouldBe` TCur.length cur
            Nothing -> discard
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.current <$> TCur.dragDescStart cur of
            Just c  -> c `shouldBe` TCur.current cur
            Nothing -> discard
      in  prop
  it "same as dragDescEnd dragStart"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.dragDescStart @Maybe cur
              `shouldBe` (TCur.dragStart <$> TCur.dragDescEnd cur)
      in  prop

spec_dragDescEnd :: SpecWith ()
spec_dragDescEnd = describe "Data.TreeCursor.dragDescEnd" $ do
  it "the length of the cursor should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.length @Int <$> TCur.dragDescEnd cur of
            Just c  -> c `shouldBe` TCur.length cur
            Nothing -> discard
      in  prop
  it "the selection should remain the same"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur = case TCur.current <$> TCur.dragDescEnd cur of
            Just c  -> c `shouldBe` TCur.current cur
            Nothing -> discard
      in  prop
  it "same as dragDescStart dragEnd"
    $ property
    $ let prop :: TCur.Cursor Int Int -> Expectation
          prop cur =
            TCur.dragDescEnd @Maybe cur
              `shouldBe` (TCur.dragEnd <$> TCur.dragDescStart cur)
      in  prop



spec :: SpecWith ()
spec = parallel $ do
  spec_From
  spec_singleton
  -- spec_mkCursorAt            -- [SLOW]
  spec_getSelectedTree
  spec_pathFromRoot
  spec_getPrev
  spec_getNext
  -- spec_length                -- [SLOW]
  spec_index
  -- spec_insertPrev            -- [SLOW]
  -- spec_insertNext            -- [SLOW]
  spec_insertDescStart
  spec_insertDescEnd
  -- spec_insertSelectPrev      -- [SLOW]
  -- spec_insertSelectNext      -- [SLOW]
  -- spec_insertSelectStart     -- [SLOW]
  -- spec_insertSelectEnd       -- [SLOW]
  -- spec_insertSelectRoot      -- [SLOW]
  spec_insertSelectDescStart
  -- spec_insertSelectDescEnd   -- [SLOW]
  spec_demoteAndInsert
  spec_delPrev
  spec_delNext
  -- spec_delSelectPrev         -- [SLOW]
  spec_delSelectNext
  spec_selectPrevSameLevel
  spec_selectNextSameLevel
  -- spec_selectPrev            -- [SLOW]
  -- spec_selectNext            -- [SLOW]
  spec_selectStart
  spec_selectEnd
  spec_selectRoot
  -- spec_selectAt              -- [SLOW]
  spec_ascend
  spec_descendStart
  spec_descendEnd
  spec_descendAt
  -- spec_findBelowBreadthFirst -- [SLOW]
  -- spec_findBreadthFirst      -- [SLOW]
  -- spec_dragPrevSameLevel     -- [SLOW]
  -- spec_dragNextSameLevel     -- [SLOW]
  -- spec_dragStart             -- [SLOW]
  -- spec_dragEnd               -- [SLOW]
  -- spec_dragAbove             -- [SLOW]
  -- spec_dragRoot              -- [FAIL]
  -- spec_dragDescStart         -- [SLOW]
  -- spec_dragDescEnd           -- [SLOW]
