{-# LANGUAGE ViewPatterns, TypeApplications, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS  -Wno-orphans #-}
module Test.Data.ListCursorSpec
  ( spec
  ) where

-- base
import           Control.Applicative

-- self
import qualified Data.ListCursor               as LCur
import           Numeric.Natural

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
-- Specs
--------------------------------------------------------------------------------

spec_from :: SpecWith ()
spec_from = describe "Data.ListCursor.from" $ do
  it "list to cursor to list does nothing"
    $ property
    $ let prop :: [Int] -> Expectation
          prop xs = via @(LCur.Cursor Int) xs `shouldBe` xs
      in  prop

spec_empty :: SpecWith ()
spec_empty = describe "Data.ListCursor.empty" $ do
  it "it is the empty list cursor" $ do
    LCur.empty
      `shouldBe` (LCur.MkCursor { LCur.prev = [], LCur.next = [] } :: LCur.Cursor
                     Int
                 )

spec_mkCursorAt :: SpecWith ()
spec_mkCursorAt = describe "Data.ListCursor.mkCursorAt" $ do
  it "0 means that all elements are in next" $ property $ \(xs :: [Int]) ->
    LCur.mkCursorAt 0 xs `shouldBe` Just (from xs)
  it "the index of mkCursorAt is the index"
    $ property
    $ let prop :: Natural -> [Int] -> Expectation
          prop i xs = case LCur.mkCursorAt i xs of
            Just theCur -> LCur.index theCur `shouldBe` i
            Nothing     -> discard
      in  prop
  it "mkCursorAt 1 should have one element previus" $ do
    LCur.mkCursorAt 1 [0 .. 2 :: Int]
      `shouldBe` Just LCur.MkCursor { LCur.prev = [0], LCur.next = [1, 2] }

spec_mkCursorClip :: SpecWith ()
spec_mkCursorClip = describe "Data.ListCursor.mkCursorClip" $ do
  it
      "mkCursorClip should be at the last position if the index exeades the length of the list"
    $ property
    $ let prop :: Natural -> [Int] -> Expectation
          prop i xs = if unsafeFrom i > length xs
            then LCur.next (LCur.mkCursorClip i xs) `shouldBe` []
            else discard
      in  prop

spec_length :: SpecWith ()
spec_length = describe "Data.ListCursor.length" $ do
  it "this is just a generic length function"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.length cur `shouldBe` length cur
      in  prop

spec_index :: SpecWith ()
spec_index = describe "Data.ListCursor.index" $ do
  it "index can be used to recreat the cursor at the some position"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur =
            LCur.mkCursorAt (LCur.index cur) (from cur) `shouldBe` Just cur
      in  prop
  it "the index always starts at 0"
    $ property
    $ let prop :: [Int] -> Expectation
          prop xs = LCur.index @Int (into @(LCur.Cursor Int) xs) `shouldBe` 0
      in  prop

spec_selectPrev :: SpecWith ()
spec_selectPrev = describe "Data.ListCursor.selectPrev" $ do
  it "selecting the previus element will subtract 1 from the index"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.selectPrev cur of
            Just x  -> LCur.index @Int x `shouldBe` LCur.index cur - 1
            Nothing -> discard
      in  prop
  it "selecting the previus element and then selecting the next does nothing"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.selectNext =<< LCur.selectPrev cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop

spec_selectNext :: SpecWith ()
spec_selectNext = describe "Data.ListCursor.selectNext" $ do
  it "selecting the next element will add 1 to the index"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.selectNext cur of
            Just x  -> LCur.index @Int x `shouldBe` LCur.index cur + 1
            Nothing -> discard
      in  prop
  it "selecting the next element and then selecting the previus does nothing"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.selectPrev =<< LCur.selectNext cur of
            Just x  -> x `shouldBe` cur
            Nothing -> discard
      in  prop

spec_selectIndex :: SpecWith ()
spec_selectIndex = describe "Data.ListCursor.selectIndex" $ do
  it "selecting the index that you are at does nothing"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.selectIndex (LCur.index cur) cur `shouldBe` Just cur
      in  prop
  it
      "selecting any index is the same as converting back to a list and then mkCursorAt desired index"
    $ property
    $ let prop :: LCur.Cursor Int -> Natural -> Expectation
          prop cur i = shouldBe @(Maybe (LCur.Cursor Int))
            (LCur.selectIndex i cur)
            (LCur.mkCursorAt i (into @[Int] cur))
      in  prop

spec_selectRelative :: SpecWith ()
spec_selectRelative = describe "Data.ListCursor.selectRelative" $ do
  it "selecting 0 does nothing"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.selectRelative 0 cur `shouldBe` Just cur
      in  prop

spec_selectStart :: SpecWith ()
spec_selectStart = describe "Data.ListCursor.selectStart" $ do
  it "the length of pred should be 0"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.prev (LCur.selectStart cur) `shouldBe` []
      in  prop

spec_selectEnd :: SpecWith ()
spec_selectEnd = describe "Data.ListCursor.selectEnd" $ do
  it "the length of next should be 0"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.next (LCur.selectEnd cur) `shouldBe` []
      in  prop

spec_prevItem :: SpecWith ()
spec_prevItem = describe "Data.ListCursor.prevItem" $ do
  it "if the first item in prev is x then prevItem should be x"
    $ property
    $ let prop :: Int -> Expectation
          prop x =
            LCur.prevItem (LCur.MkCursor { LCur.prev = [x], LCur.next = [] })
              `shouldBe` Just x
      in  prop

spec_nextItem :: SpecWith ()
spec_nextItem = describe "Data.ListCursor.nextItem" $ do
  it "if the first item in next is x then nextItem should be x"
    $ property
    $ let prop :: Int -> Expectation
          prop x =
            LCur.nextItem (LCur.MkCursor { LCur.prev = [], LCur.next = [x] })
              `shouldBe` Just x
      in  prop

spec_selectPrevUntil :: SpecWith ()
spec_selectPrevUntil = describe "Data.ListCursor.selectPrevUntil" $ do
  it "the prevItem satisfies the predicate"
    $ property
    $ let prop :: LCur.Cursor Int -> Fun Int Bool -> Expectation
          prop cur (applyFun->p) =
            case LCur.prevItem =<< LCur.selectPrevUntil p cur of
              Just x  -> p x `shouldBe` True
              Nothing -> discard
      in  prop

spec_selectNextUntil :: SpecWith ()
spec_selectNextUntil = describe "Data.ListCursor.selectNextUntil" $ do
  it "the nextItem satisfies the predicate"
    $ property
    $ let prop :: LCur.Cursor Int -> Fun Int Bool -> Expectation
          prop cur (applyFun->p) =
            case LCur.nextItem =<< LCur.selectNextUntil p cur of
              Just x  -> p x `shouldBe` True
              Nothing -> discard
      in  prop

spec_insertPrev :: SpecWith ()
spec_insertPrev = describe "Data.ListCursor.insertPrev" $ do
  it "increases the length of the cursor by 1"
    $ property
    $ let prop :: LCur.Cursor Int -> Int -> Expectation
          prop cur x = length (LCur.insertPrev x cur) `shouldBe` length cur + 1
      in  prop
  it "insertPrev and then prevItem gives back the same item"
    $ property
    $ let prop :: LCur.Cursor Int -> Int -> Expectation
          prop cur x = LCur.prevItem (LCur.insertPrev x cur) `shouldBe` Just x
      in  prop

spec_insertNext :: SpecWith ()
spec_insertNext = describe "Data.ListCursor.insertNext" $ do
  it "increases the length of the cursor by 1"
    $ property
    $ let prop :: LCur.Cursor Int -> Int -> Expectation
          prop cur x = length (LCur.insertNext x cur) `shouldBe` length cur + 1
      in  prop
  it "insertNext and then nextItem gives back the same item"
    $ property
    $ let prop :: LCur.Cursor Int -> Int -> Expectation
          prop cur x = LCur.nextItem (LCur.insertNext x cur) `shouldBe` Just x
      in  prop

spec_delPrev :: SpecWith ()
spec_delPrev = describe "Data.ListCursor.delPrev" $ do
  it "subtracts 1 from the cursor length"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.delPrev cur of
            Just x  -> length x `shouldBe` length cur - 1
            Nothing -> discard
      in  prop
  it "getting the prev item then deleting it and then insertPrev does nothing"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur =
            let curMaybe :: Maybe (LCur.Cursor Int)
                curMaybe = liftA2 LCur.insertPrev
                                  (LCur.prevItem cur)
                                  (LCur.delPrev cur)
            in  case curMaybe of
                  Just x  -> x `shouldBe` cur
                  Nothing -> discard
      in  prop

spec_delNext :: SpecWith ()
spec_delNext = describe "Data.ListCursor.delNext" $ do
  it "subtracts 1 from the cursor length"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.delNext cur of
            Just x  -> length x `shouldBe` length cur - 1
            Nothing -> discard
      in  prop
  it "getting the next item then deleting it and then insertNext does nothing"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur =
            let curMaybe :: Maybe (LCur.Cursor Int)
                curMaybe = liftA2 LCur.insertNext
                                  (LCur.nextItem cur)
                                  (LCur.delNext cur)
            in  case curMaybe of
                  Just x  -> x `shouldBe` cur
                  Nothing -> discard
      in  prop

spec_split :: SpecWith ()
spec_split = describe "Data.ListCursor.split" $ do
  it "the first cursor next is empty"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.next (fst (LCur.split cur)) `shouldBe` []
      in  prop
  it "the second cursor prev is empty"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.prev (snd (LCur.split cur)) `shouldBe` []
      in  prop
  it
      "the prev of the first cursor and the next of the second cursor can be used to make the original cursor"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.split cur of
            (x, y) ->
              LCur.MkCursor { LCur.prev = LCur.prev x, LCur.next = LCur.next y }
                `shouldBe` cur
      in  prop
  it
      "the sum of the length of both cursors is the same as the length of the original"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.split cur of
            (x, y) -> length x + length y `shouldBe` length cur
      in  prop

spec_combine :: SpecWith ()
spec_combine = describe "Data.ListCursor.combine" $ do
  it "spliting a cursor and then using combine will do nothing"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = case LCur.split cur of
            (x, y) -> LCur.combine x y `shouldBe` cur
      in  prop

spec_render :: SpecWith ()
spec_render = describe "Data.ListCursor.render" $ do
  it "render (++) is the same as from"
    $ property
    $ let prop :: LCur.Cursor Int -> Expectation
          prop cur = LCur.render (++) cur `shouldBe` from cur
      in  prop

spec_filter :: SpecWith ()
spec_filter = describe "Data.ListCursor.filter" $ do
  it
      "filtering a cursor and then using from is the same as using from and then filtering on a list"
    $ property
    $ let prop :: LCur.Cursor Int -> Fun Int Bool -> Expectation
          prop cur (applyFun->p) =
            from (LCur.filter p cur) `shouldBe` filter p (from cur)
      in  prop

spec :: SpecWith ()
spec = parallel $ do
  spec_from
  spec_empty
  spec_mkCursorAt
  spec_mkCursorClip
  spec_length
  spec_index
  spec_selectPrev
  spec_selectNext
  spec_selectIndex
  spec_selectRelative
  spec_selectStart
  spec_selectEnd
  spec_prevItem
  spec_nextItem
  spec_selectPrevUntil
  spec_selectNextUntil
  spec_insertPrev
  spec_insertNext
  spec_delPrev
  spec_delNext
  spec_split
  spec_combine
  spec_render
  spec_filter
