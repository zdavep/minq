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

tableA :: [RowA]
tableA =
  [ RowA 1 "row 1"
  , RowA 2 "row 2"
  , RowA 3 "row 3"
  , RowA 4 "row 4"
  ]

tableB :: [RowB]
tableB =
  [ RowB 4 1 True
  , RowB 3 2 False
  , RowB 2 3 True
  , RowB 1 4 False
  ]

selectAll :: [RowA] -> [RowA]
selectAll table =
  runMINQ $
    MINQ_
      (select_ id)
      table

selectEvens :: [RowA] -> [String]
selectEvens table =
  runMINQ $
    MINQ
      (select_ aName)
      table
      (where_ $ even . aId)

selectActives :: [RowB] -> [RowB]
selectActives table =
  runMINQ $
    MINQ
      (select_ id)
      table
      (where_ bActive)


joinTables :: [RowA] -> [RowB] -> [String]
joinTables a b =
  runMINQ $
    MINQ
      (select_ $ aName . fst)
      (join_ a aId b bFk)
      (where_ $ bActive . snd)

spec_select =
  describe "_select" $ do
    it "selects all rows" $
      selectAll tableA `shouldBe` tableA
    it "selects names with even id values" $
      selectEvens tableA `shouldBe` ["row 2", "row 4"]
    it "selects active rows" $
      selectActives tableB `shouldBe` [RowB 4 1 True, RowB 2 3 True]

spec_join =
  describe "_join" $
    it "joins active rows" $
      joinTables tableA tableB `shouldBe` ["row 1", "row 3"]

allSpecs =
  [ spec_select
  , spec_join
  ]

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain (testGroup "Specs" specs)
