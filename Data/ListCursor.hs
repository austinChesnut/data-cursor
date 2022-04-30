{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, DeriveGeneric, LambdaCase #-}
module Data.ListCursor
  ( Cursor(..)
  , Data.ListCursor.empty
  , mkCursorAt
  , mkCursorClip
  , Data.ListCursor.length
  , index
  , selectPrev
  , selectNext
  , selectIndex
  , selectRelative
  , selectStart
  , selectEnd
  , prevItem
  , nextItem
  , selectPrevUntil
  , selectNextUntil
  , insertPrev
  , insertNext
  , delPrev
  , delNext
  , split
  , combine
  , render
  , Data.ListCursor.filter
  ) where

-- Base
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.List                      ( genericLength )
import           Data.Maybe
import           GHC.Generics            hiding ( from )
import           Numeric.Natural

-- Witch
import           Witch

-- QuickCheck
import           Test.QuickCheck


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

maybeToAlt :: Alternative t => Maybe a -> t a
maybeToAlt = maybe Control.Applicative.empty pure

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
  foldMap fm cur =
    let fromCur = from :: Cursor a -> [a] in foldMap fm $ fromCur cur

instance From (Cursor a) [a] where
  from MkCursor { prev = ps, next = ns } = reverse ps ++ ns

instance From [a] (Cursor a) where
  from x = MkCursor { prev = [], next = x }

instance Arbitrary a => Arbitrary (Cursor a) where
  arbitrary = liftA2 MkCursor arbitrary arbitrary

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

empty :: Cursor a
empty = MkCursor { prev = [], next = [] }

mkCursorAt :: Alternative t => Natural -> [a] -> t (Cursor a)
mkCursorAt i x = selectRelative (toInteger i) $ from $ toList x

mkCursorClip :: Natural -> [a] -> Cursor a
mkCursorClip i x =
  let (ps, ns) = splitAt (fromIntegral i) (toList x)
  in  MkCursor { prev = reverse ps, next = ns }

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

selectPrev :: Alternative t => Cursor a -> t (Cursor a)
selectPrev = \case
  MkCursor { prev = (p : ps), next = ns } ->
    pure MkCursor { prev = ps, next = p : ns }
  _ -> Control.Applicative.empty

selectNext :: Alternative t => Cursor a -> t (Cursor a)
selectNext = \case
  MkCursor { prev = ps, next = (n : ns) } ->
    pure MkCursor { prev = n : ps, next = ns }
  _ -> Control.Applicative.empty

selectIndex :: Alternative t => Natural -> Cursor a -> t (Cursor a)
selectIndex i = mkCursorAt i . toList

selectRelative :: Alternative t => Integer -> Cursor a -> t (Cursor a)
selectRelative i cur = case compare i 0 of
  LT ->
    maybe Control.Applicative.empty (selectRelative (i + 1)) $ selectPrev cur
  EQ -> pure cur
  GT ->
    maybe Control.Applicative.empty (selectRelative (i - 1)) $ selectNext cur

selectStart :: Cursor a -> Cursor a
selectStart c = maybe c selectStart $ selectPrev c

selectEnd :: Cursor a -> Cursor a
selectEnd c = maybe c selectEnd $ selectNext c

selectPrevUntil :: Alternative t => (a -> Bool) -> Cursor a -> t (Cursor a)
selectPrevUntil p =
  let prevMatch = \case
        c'@MkCursor { prev = (p' : _) } | p p' -> Just c'
        _ -> Nothing
  in  maybeToAlt . (prevMatch <=< selectPrev)

selectNextUntil
  :: forall t a . Alternative t => (a -> Bool) -> Cursor a -> t (Cursor a)
selectNextUntil p =
  let nextMatch :: Cursor a -> Maybe (Cursor a)
      nextMatch = \case
        c'@MkCursor { next = (n : _) } | p n -> pure c'
        _ -> Control.Applicative.empty
  in  maybeToAlt . (nextMatch <=< selectNext)

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

insertPrev :: a -> Cursor a -> Cursor a
insertPrev x c@MkCursor { prev = ps } = c { prev = x : ps }

insertNext :: a -> Cursor a -> Cursor a
insertNext x c@MkCursor { next = ns } = c { next = x : ns }

--------------------------------------------------------------------------------
-- Deletion
--------------------------------------------------------------------------------

delPrev :: Alternative t => Cursor a -> t (Cursor a)
delPrev = \case
  MkCursor { prev = (_ : ps), next = ns } ->
    pure MkCursor { prev = ps, next = ns }
  _ -> Control.Applicative.empty

delNext :: Alternative t => Cursor a -> t (Cursor a)
delNext = \case
  MkCursor { prev = ps, next = (_ : ns) } ->
    pure MkCursor { prev = ps, next = ns }
  _ -> Control.Applicative.empty


filter :: (a -> Bool) -> Cursor a -> Cursor a
filter p MkCursor { prev = ps, next = ns } =
  MkCursor { prev = Prelude.filter p ps, next = Prelude.filter p ns }

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

length :: Num n => Cursor a -> n
length = foldl (const . (+ 1)) 0

index :: Num n => Cursor a -> n
index MkCursor { prev = ps } = genericLength ps

prevItem :: Alternative t => Cursor a -> t a
prevItem = maybe Control.Applicative.empty pure . listToMaybe . prev

nextItem :: Alternative t => Cursor a -> t a
nextItem = maybe Control.Applicative.empty pure . listToMaybe . next

split :: Cursor a -> (Cursor a, Cursor a)
split MkCursor { prev = ps, next = ns } =
  (MkCursor { prev = ps, next = [] }, MkCursor { prev = [], next = ns })

combine :: Cursor a -> Cursor a -> Cursor a
combine MkCursor { prev = ps1, next = ns1 } MkCursor { prev = ps2, next = ns2 }
  = MkCursor { prev = reverse ns1 ++ ps1, next = reverse ps2 ++ ns2 }

render :: ([a] -> [a] -> b) -> Cursor a -> b
render f MkCursor { prev = ps, next = ns } = f (reverse ps) ns
