{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, RankNTypes, LambdaCase, DeriveGeneric #-}
module Data.List.NonEmptyCursor
  ( Cursor(..)
  , mkCursorAt
  , singleton
  , index
  , Data.List.NonEmptyCursor.length
  , render
  , insertPrev
  , insertNext
  , insertPrevSelect
  , insertNextSelect
  , delPrev
  , delNext
  , delSelectPrev
  , delSelectNext
  , selectPrev
  , selectNext
  , selectFirst
  , selectLast
  , selectIndex
  , selectRelative
  , selectPrevUntil
  , selectNextUntil
  , prevItem
  , nextItem
  , dragPrev
  , dragNext
  , dragStart
  , dragEnd
  , dragTo
  , dragRelitive
  ) where

-- Witch
import           Witch

-- QuickCheck
import           Test.QuickCheck

-- base
import           Control.Applicative
import           Data.Bifunctor
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           GHC.Generics            hiding ( from )
import           Numeric.Natural



--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data Cursor a b = MkCursor
  { prev    :: [a] -- this list is reversed
  , current :: b
  , next    :: [a]
  }
  deriving (Read, Show, Eq, Ord, Generic)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Functor (Cursor a) where
  fmap f c@MkCursor { current = c' } = c { current = f c' }

instance Bifunctor Cursor where
  bimap f g MkCursor { prev = ps, current = c, next = ns } =
    MkCursor { prev = map f ps, current = g c, next = map f ns }

instance From a b => From (NE.NonEmpty a) (Cursor a b) where
  from (x NE.:| xs) = MkCursor { prev = [], current = from x, next = xs }

instance From b a => From (Cursor a b) (NE.NonEmpty a) where
  from MkCursor { prev = ps, current = c, next = ns } =
    foldl (flip NE.cons) (from c NE.:| ns) ps

instance From b a => From (Cursor a b) [a] where
  from = NE.toList . from

instance From a b => TryFrom [a] (Cursor a b) where
  tryFrom = maybeTryFrom (fmap from . NE.nonEmpty)

instance (Arbitrary a, Arbitrary b ) => Arbitrary (Cursor a b) where
  arbitrary = liftA3 MkCursor arbitrary arbitrary arbitrary

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

mkCursorAt
  :: (From a b, From b a, Alternative t)
  => Natural
  -> NE.NonEmpty a
  -> t (Cursor a b)
mkCursorAt i =
  let mkCursorAt' i' cur | i' > 0    = mkCursorAt' (i' - 1) =<< selectNext cur
                         | otherwise = Just cur
  in  maybe empty pure . mkCursorAt' i . from

singleton :: b -> Cursor a b
singleton x = MkCursor { prev = [], current = x, next = [] }

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

index :: Num n => Cursor a b -> n
index = foldr (\_ -> (+) 1) 0 . prev

length :: Num n => Cursor a b -> n
length MkCursor { prev = ps, next = ns } =
  1 + fromIntegral (Prelude.length ps) + fromIntegral (Prelude.length ns)

render :: ([a] -> b -> [a] -> c) -> Cursor a b -> c
render f MkCursor { prev = ps, current = c, next = ns } = f (reverse ps) c ns

prevItem :: Alternative t => Cursor a b -> t a
prevItem = maybe empty pure . listToMaybe . prev

nextItem :: Alternative t => Cursor a b -> t a
nextItem = maybe empty pure . listToMaybe . next

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

insertPrev :: a -> Cursor a b -> Cursor a b
insertPrev x c = c { prev = x : prev c }

insertNext :: a -> Cursor a b -> Cursor a b
insertNext x c = c { next = x : next c }

insertPrevSelect :: (From b a) => b -> Cursor a b -> Cursor a b
insertPrevSelect x MkCursor { prev = ps, current = c, next = ns } =
  MkCursor { prev = from c : ps, current = x, next = ns }

insertNextSelect :: (From b a) => b -> Cursor a b -> Cursor a b
insertNextSelect x MkCursor { prev = ps, current = c, next = ns } =
  MkCursor { prev = ps, current = x, next = from c : ns }

--------------------------------------------------------------------------------
-- Deletion
--------------------------------------------------------------------------------

delPrev :: Alternative t => Cursor a b -> t (Cursor a b)
delPrev = \case
  MkCursor { prev = _ : ps, current = c, next = ns } ->
    pure MkCursor { prev = ps, current = c, next = ns }
  _ -> empty

delNext :: Alternative t => Cursor a b -> t (Cursor a b)
delNext = \case
  MkCursor { prev = ps, current = c, next = _ : ns } ->
    pure MkCursor { prev = ps, current = c, next = ns }
  _ -> empty

delSelectPrev :: (Alternative t, From a b) => Cursor a b -> t (Cursor a b)
delSelectPrev = \case
  MkCursor { prev = p : ps, current = _, next = ns } ->
    pure MkCursor { prev = ps, current = from p, next = ns }
  _ -> empty

delSelectNext :: (Alternative t, From a b) => Cursor a b -> t (Cursor a b)
delSelectNext = \case
  MkCursor { prev = ps, current = _, next = n : ns } ->
    pure MkCursor { prev = ps, current = from n, next = ns }
  _ -> empty

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

selectPrev
  :: forall a b t
   . (Alternative t, From a b, From b a)
  => Cursor a b
  -> t (Cursor a b)
selectPrev = \case
  MkCursor { prev = p : ps, current = c, next = ns } ->
    pure MkCursor { prev = ps, current = from p, next = from c : ns }
  _ -> empty

selectNext
  :: forall a b t
   . (Alternative t, From a b, From b a)
  => Cursor a b
  -> t (Cursor a b)
selectNext = \case
  MkCursor { prev = ps, current = c, next = n : ns } ->
    pure MkCursor { prev = from c : ps, current = from n, next = ns }
  _ -> empty

selectFirst :: (From a b, From b a) => Cursor a b -> Cursor a b
selectFirst c = maybe c selectFirst $ selectPrev c

selectLast :: (From a b, From b a) => Cursor a b -> Cursor a b
selectLast c = maybe c selectLast $ selectNext c

selectIndex
  :: (Alternative t, From a b, From b a)
  => Natural
  -> Cursor a b
  -> t (Cursor a b)
selectIndex i = mkCursorAt i . from

selectRelative
  :: (Alternative t, From a b, From b a)
  => Integer
  -> Cursor a b
  -> t (Cursor a b)
selectRelative i c = case compare i 0 of
  LT -> maybe empty pure $ selectRelative (i + 1) =<< selectPrev c
  EQ -> pure c
  GT -> maybe empty pure $ selectRelative (i - 1) =<< selectNext c

selectPrevUntil
  :: (From a b, From b a, Alternative t)
  => (b -> Bool)
  -> Cursor a b
  -> t (Cursor a b)
selectPrevUntil p c = case selectPrev c of
  Just c'@MkCursor { current = theCurrent } | p theCurrent -> pure c'
  Just c' -> selectPrevUntil p c'
  _       -> empty

selectNextUntil
  :: (From a b, From b a, Alternative t)
  => (b -> Bool)
  -> Cursor a b
  -> t (Cursor a b)
selectNextUntil p c = case selectNext c of
  Just c'@MkCursor { current = theCurrent } | p theCurrent -> pure c'
  Just c' -> selectNextUntil p c'
  _       -> empty

--------------------------------------------------------------------------------
-- Drag
--------------------------------------------------------------------------------

dragPrev :: Alternative t => Cursor a b -> t (Cursor a b)
dragPrev = \case
  MkCursor { prev = p : ps, current = c, next = ns } ->
    pure MkCursor { prev = ps, current = c, next = p : ns }
  _ -> empty

dragNext :: Alternative t => Cursor a b -> t (Cursor a b)
dragNext = \case
  MkCursor { prev = ps, current = c, next = n : ns } ->
    pure MkCursor { prev = n : ps, current = c, next = ns }
  _ -> empty

dragStart :: Cursor a b -> Cursor a b
dragStart c = maybe c dragStart $ dragPrev c

dragEnd :: Cursor a b -> Cursor a b
dragEnd c = maybe c dragEnd $ dragNext c

dragTo :: Alternative t => Natural -> Cursor a b -> t (Cursor a b)
dragTo i c = dragRelitive (toInteger i - index c) c

dragRelitive :: Alternative t => Integer -> Cursor a b -> t (Cursor a b)
dragRelitive i c = case compare i 0 of
  LT -> maybe empty pure $ dragRelitive (i + 1) =<< dragPrev c
  EQ -> pure c
  GT -> maybe empty pure $ dragRelitive (i - 1) =<< dragNext c
