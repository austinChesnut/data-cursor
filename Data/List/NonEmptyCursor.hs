{-# LANGUAGE ScopedTypeVariables, RankNTypes, LambdaCase, TypeApplications, DeriveGeneric #-}
module Data.List.NonEmptyCursor
  ( Cursor(..)
  , mkCursor
  , mkCursorAt
  , singleton
  , toNonEmpty
  , index
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
  , selectPrevUntil
  , selectNextUntil
  , prevItem
  , nextItem
  , dragPrev
  , dragNext
  , dragStart
  , dragEnd
  , dragTo
  ) where

-- Witch
import           Witch

import           Control.Applicative
import           Data.Bifunctor
-- base
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           GHC.Generics            hiding ( from )



--------------------------------------------------------------------------------
-- Local Helpers
--------------------------------------------------------------------------------

listToAlts :: Alternative t => [a] -> t a
listToAlts = foldl (\alt x -> alt <|> pure x) empty

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

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

mkCursor :: (From a b) => NE.NonEmpty a -> Cursor a b
mkCursor (x NE.:| xs) = MkCursor { prev = [], current = from x, next = xs }

mkCursorAt
  :: (From a b, Alternative t, Integral n)
  => n
  -> NE.NonEmpty a
  -> t (Cursor a b)
mkCursorAt i ne = case NE.splitAt (fromIntegral i) ne of
  (ps, c : ns) ->
    pure MkCursor { prev = reverse ps, current = from c, next = ns }
  _ -> empty

singleton :: b -> Cursor a b
singleton x = MkCursor { prev = [], current = x, next = [] }

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

toNonEmpty :: (From b a) => Cursor a b -> NE.NonEmpty a
toNonEmpty MkCursor { prev = ps, current = c, next = ns } = case reverse ps of
  []         -> from c NE.:| ns
  (p' : ps') -> p' NE.:| (ps' ++ [from c] ++ ns)

index :: Num n => Cursor a b -> n
index MkCursor { prev = ps } = sum $ map (const 1) ps

render :: ([a] -> b -> [a] -> c) -> Cursor a b -> c
render f MkCursor { prev = ps, current = c, next = ns } = f (reverse ps) c ns

prevItem :: Alternative t => Cursor a b -> t a
prevItem = listToAlts . prev

nextItem :: Alternative t => Cursor a b -> t a
nextItem = listToAlts . next

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

delSelectPrev
  :: forall a b t . (From a b, Alternative t) => Cursor a b -> t (Cursor a b)
delSelectPrev = \case
  MkCursor { prev = p : ps, current = _, next = ns } ->
    let c' :: Cursor a b
        c' = MkCursor { prev = ps, current = from p, next = ns }
    in  pure c' <|> delSelectPrev c'
  _ -> empty

delSelectNext
  :: forall a b t . (From a b, Alternative t) => Cursor a b -> t (Cursor a b)
delSelectNext = \case
  MkCursor { prev = ps, current = _, next = n : ns } ->
    let c' :: Cursor a b
        c' = MkCursor { prev = ps, current = from n, next = ns }
    in  pure c' <|> delSelectNext c'
  _ -> empty

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

selectPrev
  :: forall a b t
   . (From a b, From b a, Alternative t)
  => Cursor a b
  -> t (Cursor a b)
selectPrev = \case
  MkCursor { prev = p : ps, current = c, next = ns } ->
    let c' :: Cursor a b
        c' = MkCursor { prev = ps, current = from p, next = from c : ns }
    in  pure c' <|> selectPrev c'
  _ -> empty

selectNext
  :: forall a b t
   . (From a b, From b a, Alternative t)
  => Cursor a b
  -> t (Cursor a b)
selectNext = \case
  MkCursor { prev = ps, current = c, next = n : ns } ->
    let c' :: Cursor a b
        c' = MkCursor { prev = from c : ps, current = from n, next = ns }
    in  pure c' <|> selectNext c'
  _ -> empty

selectFirst :: (From a b, From b a) => Cursor a b -> Cursor a b
selectFirst c = foldl @[] (const id) c $ selectPrev c

selectLast :: (From a b, From b a) => Cursor a b -> Cursor a b
selectLast c = foldl @[] (const id) c $ selectNext c

selectIndex
  :: (From a b, From b a, Integral n, Alternative t)
  => n
  -> Cursor a b
  -> t (Cursor a b)
selectIndex i = mkCursorAt i . toNonEmpty

selectPrevUntil
  :: (From a b, From b a, Alternative t)
  => (b -> Bool)
  -> Cursor a b
  -> t (Cursor a b)
selectPrevUntil p = listToAlts . filter (p . current) . selectPrev

selectNextUntil
  :: (From a b, From b a, Alternative t)
  => (b -> Bool)
  -> Cursor a b
  -> t (Cursor a b)
selectNextUntil p = listToAlts . filter (p . current) . selectNext

--------------------------------------------------------------------------------
-- Drag
--------------------------------------------------------------------------------

dragPrev :: Alternative t => Cursor a b -> t (Cursor a b)
dragPrev = \case
  MkCursor { prev = p : ps, current = c, next = ns } ->
    let c' = MkCursor { prev = ps, current = c, next = p : ns }
    in  pure c' <|> dragPrev c'
  _ -> empty

dragNext :: Alternative t => Cursor a b -> t (Cursor a b)
dragNext = \case
  MkCursor { prev = ps, current = c, next = n : ns } ->
    let c' = MkCursor { prev = n : ps, current = c, next = ns }
    in  pure c' <|> dragNext c'
  _ -> empty

dragStart :: Cursor a b -> Cursor a b
dragStart c = foldl @[] (const id) c $ dragPrev c

dragEnd :: Cursor a b -> Cursor a b
dragEnd c = foldl @[] (const id) c $ dragPrev c

dragTo :: (Alternative t, Integral n) => n -> Cursor a b -> t (Cursor a b)
dragTo i c = case compare i (index c) of
  LT -> maybe empty pure $ listToMaybe $ drop (fromIntegral i - 1) $ dragPrev c
  EQ -> pure c
  GT -> maybe empty pure $ listToMaybe $ drop (fromIntegral i - 1) $ dragNext c
