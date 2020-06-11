module Main where

import MINQ

import Data.Maybe       (catMaybes)
import Test.Tasty
import Test.Tasty.Hspec

data RowA = RowA
  { aId   :: Int
  , aName :: String
  }
  deriving (Eq, Show)

data RowB = RowB
  { bId     :: Int
  , bFk     :: Int
  , bActive :: Bool
  }
  deriving (Eq, Show)

-- ID select: just return all rows from a table.
selectAll :: [RowA] -> [RowA]
selectAll table =
  runMINQ $
    MINQ_ (select_ id) table

-- Testing select_ and where_
selectEvens :: [RowA] -> [String]
selectEvens table =
  runMINQ $
    MINQ (select_ aName) table (where_ $ even . aId)

-- Testing where_
selectActives :: [RowB] -> [RowB]
selectActives table =
  runMINQ $
    MINQ (select_ id) table (where_ bActive)

-- Testing join_
joinTables :: [RowA] -> [RowB] -> [String]
joinTables a b =
  runMINQ $
    MINQ
      (select_ $ aName . fst)
      (join_ a aId b bFk)
      (where_ $ bActive . snd)

-- A test table
tableA :: [RowA]
tableA =
  [ RowA 1 "row 1"
  , RowA 2 "row 2"
  , RowA 3 "row 3"
  , RowA 4 "row 4"
  ]

-- Another test table
tableB :: [RowB]
tableB =
  [ RowB 4 1 True
  , RowB 3 2 False
  , RowB 2 3 True
  , RowB 1 4 False
  ]

-- Specs for select_
spec_select =
  describe "select_" $ do
    it "selects all rows" $
      selectAll tableA `shouldBe` tableA
    it "selects names with even id values" $
      selectEvens tableA `shouldBe` ["row 2", "row 4"]
    it "selects active rows" $
      selectActives tableB `shouldBe` [RowB 4 1 True, RowB 2 3 True]

-- Specs for join_
spec_join =
  describe "join_" $
    it "joins active rows" $
      joinTables tableA tableB `shouldBe` ["row 1", "row 3"]

-- Collect all specs
allSpecs =
  [ spec_select
  , spec_join
  ]

-- Run tests
main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain (testGroup "Specs" specs)

