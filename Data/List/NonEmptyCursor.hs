{-# LANGUAGE LambdaCase, TypeApplications, DeriveGeneric #-}
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

import           Control.Applicative
import           Data.Bifunctor
-- base
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           GHC.Generics



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

mkCursor :: (a -> b) -> NE.NonEmpty a -> Cursor a b
mkCursor f (x NE.:| xs) = MkCursor { prev = [], current = f x, next = xs }

mkCursorAt
  :: (Alternative t, Integral n)
  => (a -> b)
  -> n
  -> NE.NonEmpty a
  -> t (Cursor a b)
mkCursorAt f i ne = case NE.splitAt (fromIntegral i) ne of
  (ps, c : ns) -> pure MkCursor { prev = reverse ps, current = f c, next = ns }
  _            -> empty

singleton :: b -> Cursor a b
singleton x = MkCursor { prev = [], current = x, next = [] }

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

toNonEmpty :: (b -> a) -> Cursor a b -> NE.NonEmpty a
toNonEmpty f MkCursor { prev = ps, current = c, next = ns } =
  case reverse ps of
    []         -> f c NE.:| ns
    (p' : ps') -> p' NE.:| (ps' ++ [f c] ++ ns)

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

insertPrevSelect :: (b -> a) -> b -> Cursor a b -> Cursor a b
insertPrevSelect f x MkCursor { prev = ps, current = c, next = ns } =
  MkCursor { prev = f c : ps, current = x, next = ns }

insertNextSelect :: (b -> a) -> b -> Cursor a b -> Cursor a b
insertNextSelect f x MkCursor { prev = ps, current = c, next = ns } =
  MkCursor { prev = ps, current = x, next = f c : ns }

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

delSelectPrev :: Alternative t => (a -> b) -> Cursor a b -> t (Cursor a b)
delSelectPrev f = \case
  MkCursor { prev = p : ps, current = _, next = ns } ->
    let c' = MkCursor { prev = ps, current = f p, next = ns }
    in  pure c' <|> delSelectPrev f c'
  _ -> empty

delSelectNext :: Alternative t => (a -> b) -> Cursor a b -> t (Cursor a b)
delSelectNext f = \case
  MkCursor { prev = ps, current = _, next = n : ns } ->
    let c' = MkCursor { prev = ps, current = f n, next = ns }
    in  pure c' <|> delSelectNext f c'
  _ -> empty

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

selectPrev
  :: Alternative t => (b -> a) -> (a -> b) -> Cursor a b -> t (Cursor a b)
selectPrev f g = \case
  MkCursor { prev = p : ps, current = c, next = ns } ->
    let c' = MkCursor { prev = ps, current = g p, next = f c : ns }
    in  pure c' <|> selectPrev f g c'
  _ -> empty

selectNext
  :: Alternative t => (b -> a) -> (a -> b) -> Cursor a b -> t (Cursor a b)
selectNext f g = \case
  MkCursor { prev = ps, current = c, next = n : ns } ->
    let c' = MkCursor { prev = f c : ps, current = g n, next = ns }
    in  pure c' <|> selectNext f g c'
  _ -> empty

selectFirst :: (b -> a) -> (a -> b) -> Cursor a b -> Cursor a b
selectFirst f g c = foldl @[] (const id) c $ selectPrev f g c

selectLast :: (b -> a) -> (a -> b) -> Cursor a b -> Cursor a b
selectLast f g c = foldl @[] (const id) c $ selectNext f g c

selectIndex
  :: (Integral n, Alternative t)
  => (b -> a)
  -> (a -> b)
  -> n
  -> Cursor a b
  -> t (Cursor a b)
selectIndex f g i c = mkCursorAt g i $ toNonEmpty f c

selectPrevUntil
  :: Alternative t
  => (b -> a)
  -> (a -> b)
  -> (b -> Bool)
  -> Cursor a b
  -> t (Cursor a b)
selectPrevUntil f g p = listToAlts . filter (p . current) . selectPrev f g

selectNextUntil
  :: Alternative t
  => (b -> a)
  -> (a -> b)
  -> (b -> Bool)
  -> Cursor a b
  -> t (Cursor a b)
selectNextUntil f g p = listToAlts . filter (p . current) . selectNext f g

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
