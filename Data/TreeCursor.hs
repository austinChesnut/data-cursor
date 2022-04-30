{-# LANGUAGE DeriveFunctor, TypeApplications, MultiParamTypeClasses, ScopedTypeVariables, DeriveGeneric #-}

module Data.TreeCursor
  ( Cursor(..)
  , singleton
  , mkCursorAt
  , getSelectedTree
  , pathFromRoot
  , getPrev
  , getNext
  , Data.TreeCursor.length
  , index
  , insertPrev
  , insertNext
  , insertDescStart
  , insertDescEnd
  , insertSelectPrev
  , insertSelectNext
  , insertSelectStart
  , insertSelectEnd
  , insertSelectRoot
  , insertSelectDescStart
  , insertSelectDescEnd
  , demoteAndInsert
  , delPrev
  , delNext
  , delSelectPrev
  , delSelectNext
  , selectPrevSameLevel
  , selectNextSameLevel
  , selectPrev
  , selectNext
  , selectStart
  , selectEnd
  , selectRoot
  , selectAt
  , ascend
  , descendStart
  , descendEnd
  , descendAt
  , findBelowBreadthFirst
  , findBreadthFirst
  , findBelowDepthFirst
  , findDepthFirst
  , findBelow
  , find
  , dragPrevSameLevel
  , dragNextSameLevel
  , dragStart
  , dragEnd
  , dragAbove
  , dragRoot
  , dragDescStart
  , dragDescEnd
  ) where

-- Witch
import           Witch                   hiding ( as )

-- base
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable           hiding ( find )
import           Data.List                      ( unfoldr )
import           GHC.Generics            hiding ( from )
import           Numeric.Natural

-- containers
import           Data.Tree

-- QuickCheck
import           Test.QuickCheck



--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data Cursor a b = MkCursor
  { above   :: [([Tree a], a, [Tree a])]
  , current :: b
  , below   :: [Tree a]
  }
  deriving (Show, Read, Eq, Ord, Generic, Functor)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Bifunctor Cursor where
  bimap f g c = c
    { above   = map (\(ps, c', ns) -> (map (fmap f) ps, f c', map (fmap f) ns))
                  $ above c
    , current = g $ current c
    , below   = map (fmap f) $ below c
    }

instance (From b a) => From (Cursor a b) (Tree a) where
  from MkCursor { above = as', current = c, below = bs } = foldl
    (\rec (ps, c', ns) -> Node c' $ reverse ps ++ [rec] ++ ns)
    (Node (from c) bs)
    as'

instance (From a b) => From (Tree a) (Cursor a b) where
  from (Node x xs) = MkCursor { above = [], current = from x, below = xs }

instance (Arbitrary a, Arbitrary b) => Arbitrary (Cursor a b) where
  arbitrary = liftA3 MkCursor arbitrary arbitrary arbitrary
  shrink    = genericShrink

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

singleton :: b -> Cursor a b
singleton b = MkCursor { above = [], current = b, below = [] }

mkCursorAt
  :: (From a b, From b a, Alternative t)
  => [Natural]
  -> Tree a
  -> t (Cursor a b)
mkCursorAt is c = maybe empty pure $ foldlM (flip descendAt) (from c) is

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

getSelectedTree :: (From b a) => Cursor a b -> Tree a
getSelectedTree MkCursor { current = c, below = bs } = Node (from c) bs

pathFromRoot :: (From b a) => Cursor a b -> [a]
pathFromRoot MkCursor { above = as', current = c } =
  foldr (\(_, x, _) xs -> x : xs) [] as' ++ [from c]

getPrev :: Alternative t => Cursor a b -> t a
getPrev MkCursor { above = (Node x _ : _, _, _) : _ } = pure x
getPrev _ = empty

getNext :: Alternative t => Cursor a b -> t a
getNext MkCursor { above = (_, _, Node x _ : _) : _ } = pure x
getNext _ = empty

length :: Num n => Cursor a b -> n
length MkCursor { above = as, below = bs } =
  let sumVia f xs = foldl (\x y -> x + f y) 0 xs
      lenTree (Node _ xs) = 1 + sumVia lenTree xs
      lenAbove :: Num n => n
      lenAbove = foldl
        (\x (ys1, _, ys2) -> x + sumVia lenTree ys1 + 1 + sumVia lenTree ys2)
        0
        as
      lenBelow :: Num n => n
      lenBelow = sumVia lenTree bs
  in  lenAbove + lenBelow + 1

index :: Num n => Cursor a b -> [n]
index =
  let lenList :: Num n => [a] -> n
      lenList = foldr (\_ x -> x + 1) 0
  in  foldl' (\xs (x, _, _) -> lenList x : xs) [] . above

--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

insertPrev :: Alternative t => a -> Cursor a b -> t (Cursor a b)
insertPrev x MkCursor { above = (ps, c', ns) : as', current = c, below = bs } =
  pure MkCursor { above   = (Node x [] : ps, c', ns) : as'
                , current = c
                , below   = bs
                }
insertPrev _ _ = empty

insertNext :: Alternative t => a -> Cursor a b -> t (Cursor a b)
insertNext x MkCursor { above = (ps, c', ns) : as', current = c, below = bs } =
  pure MkCursor { above   = (ps, c', Node x [] : ns) : as'
                , current = c
                , below   = bs
                }
insertNext _ _ = empty

insertDescStart :: a -> Cursor a b -> Cursor a b
insertDescStart x MkCursor { above = as', current = c, below = bs } =
  MkCursor { above = as', current = c, below = Node x [] : bs }

insertDescEnd :: a -> Cursor a b -> Cursor a b
insertDescEnd x MkCursor { above = as', current = c, below = bs } =
  MkCursor { above = as', current = c, below = bs ++ [Node x []] }

insertSelectPrev
  :: (Alternative t, From a b, From b a) => a -> Cursor a b -> t (Cursor a b)
insertSelectPrev x = maybe empty pure . (selectPrevSameLevel <=< insertPrev x)

insertSelectNext
  :: (Alternative t, From a b, From b a) => a -> Cursor a b -> t (Cursor a b)
insertSelectNext x = maybe empty pure . (selectNextSameLevel <=< insertNext x)

insertSelectStart
  :: (Alternative t, From a b, From b a) => a -> Cursor a b -> t (Cursor a b)
insertSelectStart x = insertSelectPrev x . selectStart

insertSelectEnd
  :: (Alternative t, From a b, From b a) => a -> Cursor a b -> t (Cursor a b)
insertSelectEnd x = insertSelectNext x . selectEnd

insertSelectRoot :: (From a b, From b a) => a -> Cursor a b -> Cursor a b
insertSelectRoot x = demoteAndInsert x . selectRoot

insertSelectDescStart :: (From a b, From b a) => a -> Cursor a b -> Cursor a b
insertSelectDescStart x MkCursor { above = as', current = c, below = bs } =
  MkCursor { above = ([], from c, bs) : as', current = from x, below = [] }

insertSelectDescEnd :: (From a b, From b a) => a -> Cursor a b -> Cursor a b
insertSelectDescEnd x MkCursor { above = as', current = c, below = bs } =
  MkCursor { above   = (reverse bs, from c, []) : as'
           , current = from x
           , below   = []
           }

demoteAndInsert :: (From a b, From b a) => a -> Cursor a b -> Cursor a b
demoteAndInsert x MkCursor { above = as', current = c, below = bs } =
  MkCursor { above = as', current = from x, below = [Node (from c) bs] }

--------------------------------------------------------------------------------
-- Deletion
--------------------------------------------------------------------------------

delPrev :: Alternative t => Cursor a b -> t (Cursor a b)
delPrev MkCursor { above = (_ : ps, c', ns) : as', current = c, below = bs } =
  pure MkCursor { above = (ps, c', ns) : as', current = c, below = bs }
delPrev _ = empty

delNext :: Alternative t => Cursor a b -> t (Cursor a b)
delNext MkCursor { above = (ps, c', _ : ns) : as', current = c, below = bs } =
  pure MkCursor { above = (ps, c', ns) : as', current = c, below = bs }
delNext _ = empty

delSelectPrev
  :: (Alternative t, From a b, From b a) => Cursor a b -> t (Cursor a b)
delSelectPrev = maybe empty pure . (delNext <=< selectPrev)

delSelectNext
  :: (Alternative t, From a b, From b a) => Cursor a b -> t (Cursor a b)
delSelectNext = maybe empty pure . (delPrev <=< selectNext)

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

selectPrevSameLevel
  :: forall t a b
   . (Alternative t, From a b, From b a)
  => Cursor a b
  -> t (Cursor a b)
selectPrevSameLevel MkCursor { above = (Node x xs : ps, c', ns) : as', current = c, below = bs }
  = let newCur :: Cursor a b
        newCur = MkCursor { above   = (ps, c', Node (from c) bs : ns) : as'
                          , current = from x
                          , below   = xs
                          }
    in  pure newCur <|> selectPrevSameLevel newCur
selectPrevSameLevel _ = empty

selectNextSameLevel
  :: forall t a b
   . (Alternative t, From a b, From b a)
  => Cursor a b
  -> t (Cursor a b)
selectNextSameLevel MkCursor { above = (ps, c', Node x xs : ns) : as', current = c, below = bs }
  = let newCur :: Cursor a b
        newCur = MkCursor { above   = (Node (from c) bs : ps, c', ns) : as'
                          , current = from x
                          , below   = xs
                          }
    in  pure newCur <|> selectNextSameLevel newCur
selectNextSameLevel _ = empty

selectPrev
  :: (Alternative t, From a b, From b a) => Cursor a b -> t (Cursor a b)
selectPrev c =
  let recEnd c' = maybe c' recEnd (descendEnd c')
  in  maybe empty pure $ (recEnd <$> selectPrevSameLevel c) <|> ascend c


selectNext
  :: (Alternative t, From a b, From b a) => Cursor a b -> t (Cursor a b)
selectNext c =
  let recAscend :: (From a b, From b a) => Cursor a b -> Maybe (Cursor a b)
      recAscend = ascend >=> \x -> selectNextSameLevel x <|> recAscend x
  in  maybe empty pure
        $   descendStart c
        <|> selectNextSameLevel c
        <|> recAscend c

selectStart :: (From a b, From b a) => Cursor a b -> Cursor a b
selectStart c = maybe c selectStart $ selectPrevSameLevel c

selectEnd :: (From a b, From b a) => Cursor a b -> Cursor a b
selectEnd c = maybe c selectEnd $ selectNextSameLevel c

selectRoot :: forall a b . (From a b, From b a) => Cursor a b -> Cursor a b
selectRoot = via @(Tree a)

selectAt
  :: (Alternative t, From a b, From b a)
  => [Natural]
  -> Cursor a b
  -> t (Cursor a b)
selectAt is = mkCursorAt is . from

ascend :: (Alternative t, From a b, From b a) => Cursor a b -> t (Cursor a b)
ascend MkCursor { above = (ps, c', ns) : as', current = c, below = bs } =
  pure $ MkCursor { above   = as'
                  , current = from c'
                  , below   = reverse ps ++ [Node (from c) bs] ++ ns
                  }
ascend _ = empty

descendStart
  :: (Alternative t, From a b, From b a) => Cursor a b -> t (Cursor a b)
descendStart = descendAt 0

descendEnd
  :: (Alternative t, From a b, From b a) => Cursor a b -> t (Cursor a b)
descendEnd c = maybe empty pure $ selectEnd <$> descendStart c

descendAt
  :: (Alternative t, From a b, From b a)
  => Natural
  -> Cursor a b
  -> t (Cursor a b)
descendAt i MkCursor { above = as', current = c, below = bs }
  | i < 0 = empty
  | otherwise = case splitAt (fromIntegral i) bs of
    (xs1, (Node x1 xs3) : xs2) -> pure MkCursor
      { above   = (reverse xs1, from c, xs2) : as'
      , current = from x1
      , below   = xs3
      }
    _ -> empty

findBelowBreadthFirst
  :: forall a b
   . (From a b, From b a)
  => (b -> Bool)
  -> Cursor a b
  -> [Cursor a b]
{-
findBelowBreadthFirst p =
  let allPosBelow :: Cursor a b -> Tree (Cursor a b)
      allPosBelow = unfoldTree $ liftA2 (,) id allPosBelowLevel

      allPosBelowLevel :: Cursor a b -> [Cursor a b]
      allPosBelowLevel =
        unfoldr (fmap (liftA2 (,) id selectNextSameLevel)) . descendStart
  in  filter (p . current) . flatten . allPosBelow
-}
findBelowBreadthFirst = findBelow flatten

findBreadthFirst
  :: (From a b, From b a) => (b -> Bool) -> Cursor a b -> [Cursor a b]
findBreadthFirst = find flatten

findBelowDepthFirst
  :: (From a b, From b a) => (b -> Bool) -> Cursor a b -> [Cursor a b]
{-
findBelowDepthFirst p =
  let allPosBelow :: (From a b, From b a) => Cursor a b -> Tree (Cursor a b)
      allPosBelow = unfoldTree $ liftA2 (,) id allPosBelowLevel

      allPosBelowLevel :: (From a b, From b a) => Cursor a b -> [Cursor a b]
      allPosBelowLevel =
        unfoldr (fmap (liftA2 (,) id selectNextSameLevel)) . descendStart

      flattenDepthFirst :: Tree a -> [a]
      flattenDepthFirst (Node x xs) = x : concatMap flattenDepthFirst xs
  in  filter (p . current) . flattenDepthFirst . allPosBelow
-}
findBelowDepthFirst =
  let flattenDepthFirst :: Tree a -> [a]
      flattenDepthFirst (Node x xs) = x : concatMap flattenDepthFirst xs
  in  findBelow flattenDepthFirst

findDepthFirst
  :: (From a b, From b a) => (b -> Bool) -> Cursor a b -> [Cursor a b]
findDepthFirst p = findBelowDepthFirst p . selectRoot

findBelow
  :: (From a b, From b a)
  => (Tree (Cursor a b) -> [Cursor a b])
  -> (b -> Bool)
  -> Cursor a b
  -> [Cursor a b]
findBelow f p =
  let
    allPosBelow :: (From a b, From b a) => Cursor a b -> Tree (Cursor a b)
    allPosBelow = unfoldTree $ liftA2 (,) id allPosBelowLevel
    allPosBelowLevel :: (From a b, From b a) => Cursor a b -> [Cursor a b]
    allPosBelowLevel = allPosNext . descendStart
    allPosNext :: (From a b, From b a) => Maybe (Cursor a b) -> [Cursor a b]
    allPosNext = unfoldr (fmap (liftA2 (,) id selectNextSameLevel))
  in
    filter (p . current) . f . allPosBelow

find
  :: (From a b, From b a)
  => (Tree (Cursor a b) -> [Cursor a b])
  -> (b -> Bool)
  -> Cursor a b
  -> [Cursor a b]
find f p = findBelow f p . selectRoot

--------------------------------------------------------------------------------
-- Drag
--------------------------------------------------------------------------------

dragPrevSameLevel :: Alternative t => Cursor a b -> t (Cursor a b)
dragPrevSameLevel MkCursor { above = (p : ps, c', ns) : as', current = c, below = bs }
  = pure MkCursor { above = (ps, c', p : ns) : as', current = c, below = bs }
dragPrevSameLevel _ = empty

dragNextSameLevel :: Alternative t => Cursor a b -> t (Cursor a b)
dragNextSameLevel MkCursor { above = (ps, c', n : ns) : as', current = c, below = bs }
  = pure MkCursor { above = (n : ps, c', ns) : as', current = c, below = bs }
dragNextSameLevel _ = empty

dragStart :: Cursor a b -> Cursor a b
dragStart c = maybe c dragStart $ dragPrevSameLevel c

dragEnd :: Cursor a b -> Cursor a b
dragEnd c = maybe c dragEnd $ dragNextSameLevel c

dragAbove :: Alternative t => Cursor a b -> t (Cursor a b)
dragAbove MkCursor { above = (ps1, c', ns1) : (ps2, c'', ns2) : as', current = c, below = bs }
  = pure MkCursor { above = (ps2, c'', Node c' (reverse ps1 ++ ns1) : ns2) : as'
                  , current = c
                  , below = bs
                  }
dragAbove _ = empty

dragRoot :: Alternative t => Cursor a b -> t (Cursor a b)
dragRoot c@MkCursor { above = [] } = pure c
dragRoot c@MkCursor { above = [(ps, a, ns)], below = [] } =
  pure $ c { above = [], below = reverse ps ++ [Node a []] ++ ns }
dragRoot c@MkCursor { below = [] } = maybe (pure c) dragRoot $ dragAbove c
dragRoot _                         = empty

dragDescStart :: Alternative t => Cursor a b -> t (Cursor a b)
dragDescStart MkCursor { above = (Node x xs : ps, c', ns) : as', current = c, below = bs }
  = pure MkCursor { above   = ([], x, xs) : (ps, c', ns) : as'
                  , current = c
                  , below   = bs
                  }
dragDescStart _ = empty

dragDescEnd :: Alternative t => Cursor a b -> t (Cursor a b)
dragDescEnd c = dragEnd <$> dragDescStart c
