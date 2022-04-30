{-# LANGUAGE ViewPatterns, TypeApplications, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS -Wno-orphans #-}
module Test.Data.List.NonEmptyCursorSpec
  ( spec
  ) where

-- base
import           Control.Applicative
import qualified Data.List.NonEmpty            as NE
import           Numeric.Natural

-- self
import qualified Data.List.NonEmptyCursor      as NECur

-- Witch
import           Witch

-- QuickCheck
import           Test.QuickCheck

-- Hspec
import           Test.Hspec



--------------------------------------------------------------------------------
-- Orphans
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = do
    x  <- arbitrary
    xs <- arbitrary
    pure $ x NE.:| xs

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

--------------------------------------------------------------------------------
-- Specs
--------------------------------------------------------------------------------

spec_from :: SpecWith ()
spec_from =
  describe "Data.List.NonEmptyCursor.from"
    $ it "a NonEmpty converted to a cursor and back again does nothing"
    $ property
    $ let prop :: NE.NonEmpty Int -> Expectation
          prop ne = via @(NECur.Cursor Int Int) ne `shouldBe` ne
      in  prop

spec_mkCursorAt :: SpecWith ()
spec_mkCursorAt =
  describe "Data.List.NonEmptyCursor.mkCursorAt"
    $ it
        "the index of the new cursor will be the same as then number provided to mkCursorAt"
    $ property
    $ let prop :: Natural -> NE.NonEmpty Int -> Expectation
          prop i ne =
            let cur = NECur.mkCursorAt i ne :: Maybe (NECur.Cursor Int Int)
            in  case cur of
                  Just x  -> NECur.index x `shouldBe` i
                  Nothing -> discard
      in  prop

spec_singleton :: SpecWith ()
spec_singleton = describe "Data.List.NonEmptyCursor.singleton" $ do
    -- :: b -> Cursor a b
  it
      "if you convert to a singleton and then use from it is the same as using NonEmpty singleton"
    $ property
    $ let prop :: Int -> Expectation
          prop x =
            let cur = NECur.singleton x :: NECur.Cursor Int Int
            in  from cur `shouldBe` x NE.:| []
      in  prop

spec_index :: SpecWith ()
spec_index = describe "Data.List.NonEmptyCursor.index" $ do
  it
      "if a cursor is converted to a NonEmpty then you can use the index to recreate it"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.mkCursorAt (NECur.index cur) (from cur) `shouldBe` Just cur
      in  prop

spec_length :: SpecWith ()
spec_length = describe "Data.List.NonEmptyCursor.length" $ do
  it "same as converting to a NonEmpty and then getting the length"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.length cur `shouldBe` length (into @(NE.NonEmpty Int) cur)
      in  prop

spec_render :: SpecWith ()
spec_render = describe "Data.List.NonEmptyCursor.render" $ do
  it "\\x y z -> x++[y]++z should be the same as using from"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.render (\x y z -> x ++ [y] ++ z) cur `shouldBe` from cur
      in  prop

spec_prevItem :: SpecWith ()
spec_prevItem = describe "Data.List.NonEmptyCursor.prevItem" $ do
  it "if you insertPrev and then prevItem you select the original item"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            NECur.current
              <$>        NECur.selectPrev (NECur.insertPrev x cur)
              `shouldBe` Just x
      in  prop

spec_nextItem :: SpecWith ()
spec_nextItem = describe "Data.List.NonEmptyCursor.nextItem" $ do
  it "if you insertNext and then nextItem you get back the original item"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            NECur.current
              <$>        NECur.selectNext (NECur.insertNext x cur)
              `shouldBe` Just x
      in  prop

spec_insertPrev :: SpecWith ()
spec_insertPrev = describe "Data.List.NonEmptyCursor.insertPrev" $ do
  it "increases the size of the cursor by 1"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            NECur.length @Int (NECur.insertPrev x cur)
              `shouldBe` NECur.length cur
              +          1
      in  prop

spec_insertNext :: SpecWith ()
spec_insertNext = describe "Data.List.NonEmptyCursor.insertNext" $ do
  it "increases the size of the cursor by 1"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            NECur.length @Int (NECur.insertNext x cur)
              `shouldBe` NECur.length cur
              +          1
      in  prop

spec_insertPrevSelect :: SpecWith ()
spec_insertPrevSelect =
  describe "Data.List.NonEmptyCursor.insertPrevSelect" $ do
    it "insertPrevSelect then selectPrev then delNext does nothing"
      $ property
      $ let
          prop :: NECur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            (NECur.delNext =<< NECur.selectPrev (NECur.insertPrevSelect x cur))
              `shouldBe` Just cur
        in
          prop
    it "the selected item is the what you just inserted"
      $ property
      $ let prop :: NECur.Cursor Int Int -> Int -> Expectation
            prop cur x =
              NECur.current (NECur.insertPrevSelect x cur) `shouldBe` x
        in  prop

spec_insertNextSelect :: SpecWith ()
spec_insertNextSelect =
  describe "Data.List.NonEmptyCursor.insertNextSelect" $ do
    it "insertNextSelect then selectNext then delPrev does nothing"
      $ property
      $ let
          prop :: NECur.Cursor Int Int -> Int -> Expectation
          prop cur x =
            (NECur.delPrev =<< NECur.selectNext (NECur.insertNextSelect x cur))
              `shouldBe` Just cur
        in
          prop
    it "the selected item is the what you just inserted"
      $ property
      $ let prop :: NECur.Cursor Int Int -> Int -> Expectation
            prop cur x =
              NECur.current (NECur.insertNextSelect x cur) `shouldBe` x
        in  prop

spec_delPrev :: SpecWith ()
spec_delPrev = describe "Data.List.NonEmptyCursor.delPrev" $ do
  it "prevItem then delPrev then insertPrev does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            case
                liftA2 NECur.insertPrev (NECur.prevItem cur) (NECur.delPrev cur)
              of
                Just c  -> c `shouldBe` cur
                Nothing -> discard
      in  prop
  it "subtracts 1 from the size of the cursor"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.delPrev cur of
            Just x  -> NECur.length @Int x `shouldBe` NECur.length cur - 1
            Nothing -> discard
      in  prop

spec_delNext :: SpecWith ()
spec_delNext = describe "Data.List.NonEmptyCursor.delNext" $ do
  it "nextItem then delNext then insertNext does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            case
                liftA2 NECur.insertNext (NECur.nextItem cur) (NECur.delNext cur)
              of
                Just x  -> x `shouldBe` cur
                Nothing -> discard
      in  prop
  it "subtracts 1 from the size of the cursor"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.delNext cur of
            Just x  -> NECur.length @Int x `shouldBe` NECur.length cur - 1
            Nothing -> discard
      in  prop

spec_delSelectPrev :: SpecWith ()
spec_delSelectPrev = describe "Data.List.NonEmptyCursor.delSelectPrev" $ do
  it "same as selectPrev delNext"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.delSelectPrev @Maybe cur
              `shouldBe` (NECur.delNext =<< NECur.selectPrev cur)
      in  prop

spec_delSelectNext :: SpecWith ()
spec_delSelectNext = describe "Data.List.NonEmptyCursor.delSelectNext" $ do
  it "same as selectNext delPrev"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.delSelectNext @Maybe cur
              `shouldBe` (NECur.delPrev =<< NECur.selectNext cur)
      in  prop

spec_selectPrev :: SpecWith ()
spec_selectPrev = describe "Data.List.NonEmptyCursor.selectPrev" $ do
  it "subtracts 1 from the index"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.selectPrev cur of
            Just x  -> NECur.index @Int x `shouldBe` NECur.index cur - 1
            Nothing -> discard
      in  prop
  it "selectPrev then selectNext does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.selectNext =<< NECur.selectPrev cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop

spec_selectNext :: SpecWith ()
spec_selectNext = describe "Data.List.NonEmptyCursor.selectNext" $ do
  it "adds 1 to the index"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.selectNext cur of
            Just x  -> NECur.index @Int x `shouldBe` NECur.index cur + 1
            Nothing -> discard
      in  prop
  it "selectNext then selectPrev does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.selectPrev =<< NECur.selectNext cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop

spec_selectFirst :: SpecWith ()
spec_selectFirst = describe "Data.List.NonEmptyCursor.selectFirst" $ do
  it "the index should be 0"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = NECur.index @Int (NECur.selectFirst cur) `shouldBe` 0
      in  prop
  it "con't selectPrev"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.selectPrev (NECur.selectFirst cur) `shouldBe` Nothing
      in  prop

spec_selectLast :: SpecWith ()
spec_selectLast = describe "Data.List.NonEmptyCursor.selectLast" $ do
  it "can't selectNext"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = NECur.selectNext (NECur.selectLast cur) `shouldBe` Nothing
      in  prop

spec_selectIndex :: SpecWith ()
spec_selectIndex = describe "Data.List.NonEmptyCursor.selectIndex" $ do
  it "selecting 0 is the same as selectStart"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.selectIndex 0 cur `shouldBe` Just (NECur.selectFirst cur)
      in  prop
  it "selecting the current index does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.selectIndex (NECur.index cur) cur `shouldBe` Just cur
      in  prop

spec_selectRelative :: SpecWith ()
spec_selectRelative = describe "Data.List.NonEmptyCursor.selectRelative" $ do
  it "selecting 0 does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = NECur.selectRelative 0 cur `shouldBe` Just cur
      in  prop
  it "selecting 1 is the same as using selectNext"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.selectRelative @Maybe 1 cur `shouldBe` NECur.selectNext cur
      in  prop
  it "selecting -1 is the same as using selectPrev"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.selectRelative @Maybe (-1) cur `shouldBe` NECur.selectPrev cur
      in  prop

spec_selectPrevUntil :: SpecWith ()
spec_selectPrevUntil = describe "Data.List.NonEmptyCursor.selectPrevUntil" $ do
  it "the selection should satisfy the predicate"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Fun Int Bool -> Bool
          prop cur (applyFun->p) = case NECur.selectPrevUntil p cur of
            Just x  -> p (NECur.current x)
            Nothing -> discard
      in  prop
  it "the new index must less than the original"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Fun Int Bool -> Bool
          prop cur (applyFun->p) = case NECur.selectPrevUntil p cur of
            Just x  -> NECur.index @Int x < NECur.index cur
            Nothing -> discard
      in  prop

spec_selectNextUntil :: SpecWith ()
spec_selectNextUntil = describe "Data.List.NonEmptyCursor.selectNextUntil" $ do
  it "the selection should satisfy the predicate"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Fun Int Bool -> Bool
          prop cur (applyFun->p) = case NECur.selectNextUntil p cur of
            Just x  -> p (NECur.current x)
            Nothing -> discard
      in  prop
  it "the new index must greater than the original"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Fun Int Bool -> Bool
          prop cur (applyFun->p) = case NECur.selectNextUntil p cur of
            Just x  -> NECur.index @Int x > NECur.index cur
            Nothing -> discard
      in  prop

spec_dragPrev :: SpecWith ()
spec_dragPrev = describe "Data.List.NonEmptyCursor.dragPrev" $ do
  it "dragPrev then dragNext does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragNext =<< NECur.dragPrev cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop
  it "the selection remains the same"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragPrev cur of
            Just x  -> NECur.current x `shouldBe` NECur.current cur
            Nothing -> discard
      in  prop
  it "subtracts 1 from the index"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragPrev cur of
            Just x  -> NECur.index @Int x `shouldBe` NECur.index cur - 1
            Nothing -> discard
      in  prop

spec_dragNext :: SpecWith ()
spec_dragNext = describe "Data.List.NonEmptyCursor.dragNext" $ do
  it "dragNext then dragPrev does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragNext =<< NECur.dragPrev cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop
  it "the selection remains the same"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragNext cur of
            Just x  -> NECur.current x `shouldBe` NECur.current cur
            Nothing -> discard
      in  prop
  it "adds 1 to the index"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragNext cur of
            Just x  -> NECur.index @Int x `shouldBe` NECur.index cur + 1
            Nothing -> discard
      in  prop

spec_dragStart :: SpecWith ()
spec_dragStart = describe "Data.List.NonEmptyCursor.dragStart" $ do
  it "the index should be 0"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = NECur.index @Int (NECur.dragStart cur) `shouldBe` 0
      in  prop
  it "can't selectPrev"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = NECur.selectPrev (NECur.dragStart cur) `shouldBe` Nothing
      in  prop
  it "the selection remains the same"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.current (NECur.dragStart cur) `shouldBe` NECur.current cur
      in  prop
  it "the new index is lessthan or equal to the original index"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Bool
          prop cur = NECur.index @Int (NECur.dragStart cur) <= NECur.index cur
      in  prop

dragEnd :: SpecWith ()
dragEnd = describe "Data.List.NonEmptyCursor.dragEnd" $ do
  it "can't selectNext"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = NECur.selectNext (NECur.dragEnd cur) `shouldBe` Nothing
      in  prop
  it "the selection remains the same"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur =
            NECur.current (NECur.dragEnd cur) `shouldBe` NECur.current cur
      in  prop
  it "the new index is greaterthann or equal to the original index"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Bool
          prop cur = NECur.index @Int (NECur.dragEnd cur) >= NECur.index cur
      in  prop

spec_dragTo :: SpecWith ()
spec_dragTo = describe "Data.List.NonEmptyCursor.dragTo" $ do
  it "dragTo 0 is the same as dragStart"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragTo 0 cur of
            Just x  -> x `shouldBe` NECur.dragStart cur
            Nothing -> discard
      in  prop
  it "the selection remains the same"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Natural -> Expectation
          prop cur i = case NECur.dragTo i cur of
            Just x  -> NECur.current x `shouldBe` NECur.current cur
            Nothing -> discard
      in  prop
  it "the index is the same as the number provided"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Natural -> Expectation
          prop cur i = case NECur.dragTo i cur of
            Just x  -> NECur.index x `shouldBe` i
            Nothing -> discard
      in  prop
  it "dragTo the current index does nothing"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragTo (NECur.index cur) cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop

spec_dragRelitive :: SpecWith ()
spec_dragRelitive = describe "Data.List.NonEmptyCursor.dragRelitive" $ do
  it "the selection remains the same"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Integer -> Expectation
          prop cur i = case NECur.dragRelitive i cur of
            Just x  -> NECur.current x `shouldBe` NECur.current cur
            Nothing -> discard
      in  prop
  it "if the provided number is 0 then nothing changes"
    $ property
    $ let prop :: NECur.Cursor Int Int -> Expectation
          prop cur = case NECur.dragRelitive 0 cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop


spec :: SpecWith ()
spec = parallel $ do
  spec_from
  spec_mkCursorAt
  spec_singleton
  spec_index
  spec_length
  spec_render
  spec_prevItem
  spec_nextItem
  spec_insertPrev
  spec_insertNext
  spec_insertPrevSelect
  spec_insertNextSelect
  spec_delPrev
  spec_delNext
  spec_delSelectPrev
  spec_delSelectNext
  spec_selectPrev
  spec_selectNext
  spec_selectFirst
  spec_selectLast
  spec_selectIndex
  spec_selectRelative
  spec_selectPrevUntil
  spec_selectNextUntil
  spec_dragPrev
  spec_dragNext
  spec_dragStart
  dragEnd
  spec_dragTo
  spec_dragRelitive
