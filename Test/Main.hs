-- {-# LANGUAGE TypeApplications, MultiParamTypeClasses, ScopedTypeVariables #-}
module Main
  ( main
  ) where

-- self
import qualified Test.Data.List.NonEmptyCursorSpec
                                               as NonEmptyCursor
import qualified Test.Data.ListCursorSpec      as ListCursor
import qualified Test.Data.TreeCursorSpec      as TreeCursor

-- Hspec
import           Test.Hspec



main :: IO ()
main = hspec $ parallel $ do
  NonEmptyCursor.spec
  ListCursor.spec
  TreeCursor.spec
