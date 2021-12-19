{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Data.ListCursor
  ( Cursor(..)
  , Data.ListCursor.empty
  , mkCursor
  , mkCursorAt
  , mkCursorClip
  , Data.ListCursor.length
  , index
  , selectPrev
  , selectNext
  , selectIndexAt
  , selectStart
  , selectEnd
  , prevItem
  , nextItem
  , selectPrevUntil
  , selectNextUntil
  , insertPrev
  , insertNext
  , insertPrevF
  , insertNextF
  , delPrev
  , delNext
  , split
  , combine
  , render
  , Data.ListCursor.filter
  ) where

-- Base
import           Control.Applicative
import           Data.Foldable
import           Data.List                      ( genericLength )
import           Data.Maybe
import           GHC.Generics



--------------------------------------------------------------------------------
-- Local Helpers
--------------------------------------------------------------------------------

listToAlt :: Alternative t => [a] -> t a
listToAlt = foldl (\alt x -> alt <|> pure x) Control.Applicative.empty

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data Cursor a = MkCursor
  { prev :: [a] -- this list is reversed
  , next :: [a]
  }
  deriving (Show, Read, Ord, Eq, Generic)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Functor Cursor where
  fmap f MkCursor { prev = p, next = n } =
    MkCursor { prev = map f p, next = map f n }

instance Foldable Cursor where
  foldMap fm MkCursor { prev = ps, next = ns } =
    foldMap fm (reverse ps) <> foldMap fm ns

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

empty :: Cursor a
empty = MkCursor { prev = [], next = [] }

mkCursor :: Foldable t => t a -> Cursor a
mkCursor x = MkCursor { prev = [], next = toList x }

--------------------------------------------------------------------------------
-- Creation At Index
--------------------------------------------------------------------------------

mkCursorAt
  :: (Foldable t, Integral n, Alternative f) => n -> t a -> f (Cursor a)
mkCursorAt i x =
  let (ps, ns) = splitAt (fromIntegral i) l
      l        = toList x
  in  if (i < 0) && (i > genericLength l)
        then Control.Applicative.empty
        else pure $ MkCursor { prev = reverse ps, next = ns }

mkCursorClip :: (Foldable t, Integral n) => n -> t a -> Cursor a
mkCursorClip i x =
  let (ps, ns) = splitAt (fromIntegral i) (toList x)
  in  MkCursor { prev = reverse ps, next = ns }

-- mkCursorMod :: (Foldable t, Integral n) => n -> t a -> Cursor a


--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

length :: Num n => Cursor a -> n
length = foldl (const . (+ 1)) 0

index :: Num n => Cursor a -> n
index MkCursor { prev = ps } = genericLength ps

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

selectPrev :: Alternative t => Cursor a -> t (Cursor a)
selectPrev = \case
  MkCursor { prev = (p : ps), next = ns } ->
    let c = MkCursor { prev = ps, next = p : ns } in pure c <|> selectPrev c
  _ -> Control.Applicative.empty

selectNext :: Alternative t => Cursor a -> t (Cursor a)
selectNext = \case
  MkCursor { prev = ps, next = (n : ns) } ->
    let c = MkCursor { prev = n : ps, next = ns } in pure c <|> selectNext c
  _ -> Control.Applicative.empty

selectIndexAt :: (Integral n, Alternative t) => n -> Cursor a -> t (Cursor a)
selectIndexAt i = mkCursorAt i . toList

selectStart :: Cursor a -> Cursor a
selectStart = fromMaybe <*> listToMaybe . reverse . selectPrev

selectEnd :: Cursor a -> Cursor a
selectEnd = fromMaybe <*> listToMaybe . reverse . selectNext

selectPrevUntil :: Alternative t => (a -> Bool) -> Cursor a -> t (Cursor a)
selectPrevUntil p c =
  let isPrevMatch = \case
        MkCursor { prev = (p' : _) } -> p p'
        _                            -> False
  in  listToAlt $ Prelude.filter isPrevMatch $ selectPrev c

selectNextUntil :: Alternative t => (a -> Bool) -> Cursor a -> t (Cursor a)
selectNextUntil p c =
  let isNextMatch = \case
        MkCursor { next = (n : _) } -> p n
        _                           -> False
  in  listToAlt $ Prelude.filter isNextMatch $ selectNext c

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

insertPrev :: a -> Cursor a -> Cursor a
insertPrev x = insertPrevF [x]

insertNext :: a -> Cursor a -> Cursor a
insertNext x = insertNextF [x]

insertPrevF :: Foldable t => t a -> Cursor a -> Cursor a
insertPrevF x c = c { prev = toList x ++ prev c }

insertNextF :: Foldable t => t a -> Cursor a -> Cursor a
insertNextF x c = c { next = toList x ++ next c }

--------------------------------------------------------------------------------
-- Deletion
--------------------------------------------------------------------------------

delPrev :: Alternative t => Cursor a -> t (Cursor a)
delPrev = \case
  MkCursor { prev = (_ : ps), next = ns } ->
    let c = MkCursor { prev = ps, next = ns } in pure c <|> delPrev c
  _ -> Control.Applicative.empty

delNext :: Alternative t => Cursor a -> t (Cursor a)
delNext = \case
  MkCursor { prev = ps, next = (_ : ns) } ->
    let c = MkCursor { prev = ps, next = ns } in pure c <|> delNext c
  _ -> Control.Applicative.empty

--------------------------------------------------------------------------------
-- Misc 2
--------------------------------------------------------------------------------

prevItem :: Alternative t => Cursor a -> t a
prevItem = listToAlt . prev

nextItem :: Alternative t => Cursor a -> t a
nextItem = listToAlt . next

split :: Cursor a -> (Cursor a, Cursor a)
split MkCursor { prev = ps, next = ns } =
  (MkCursor { prev = ps, next = [] }, MkCursor { prev = [], next = ns })

combine :: Cursor a -> Cursor a -> Cursor a
combine MkCursor { prev = ps1, next = ns1 } MkCursor { prev = ps2, next = ns2 }
  = MkCursor { prev = reverse ns1 ++ ps1, next = reverse ps2 ++ ns2 }

render :: ([a] -> [a] -> b) -> Cursor a -> b
render f MkCursor { prev = ps, next = ns } = f (reverse ps) ns

filter :: (a -> Bool) -> Cursor a -> Cursor a
filter p MkCursor { prev = ps, next = ns } =
  MkCursor { prev = Prelude.filter p ps, next = Prelude.filter p ns }
