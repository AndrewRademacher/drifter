{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import           Control.Applicative
import           Data.IORef
import           Data.List
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Drifter.Graph
import           Drifter.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "drifter"
    [
      graphTests
    , typesTests
    ]

graphTests :: TestTree
graphTests = testGroup "Drifter.Graph"
    [
      resolveDependencyOrderTests
    ]

typesTests :: TestTree
typesTests = testGroup "Drifter.Types"
    [
      migrateTests
    ]

resolveDependencyOrderTests :: TestTree
resolveDependencyOrderTests = testGroup "resolveDependencyOrder"
    [
      testProperty "orders by dependency" $ \(UniqueChanges cs) ->
        let presorted = changeSequence cs
        in resolveDependencyOrder presorted === presorted
    ]

migrateTests :: TestTree
migrateTests = testGroup "migrate"
  [
    testCase "runs the given migrations" $ do
        runs <- newIORef []
        _ <- migrate (DBConnection $ TestDBConn runs) exampleChanges
        res <- readIORef runs
        res @?= [1,2,3]
  ]

exampleChanges :: [Change TestDB]
exampleChanges = [c2, c3, c1]
  where
    c1 = Change "c1" Nothing [] (RunMigrationNumber 1)
    c2 = c1 { changeName = "c2", changeDependencies = ["c1"], changeMethod = RunMigrationNumber 2}
    c3 = c1 { changeName = "c3", changeDependencies = ["c2"], changeMethod = RunMigrationNumber 3}

data TestDB

newtype TestDBConn = TestDBConn (IORef [Int])

data instance Method TestDB = RunMigrationNumber Int deriving (Show, Eq)

data instance DBConnection TestDB = DBConnection TestDBConn

instance Drifter TestDB where
    migrate (DBConnection (TestDBConn runs)) changes = do
        let mns = map ((\(RunMigrationNumber mn) -> mn) . changeMethod) (resolveDependencyOrder changes)
        modifyIORef' runs (++ mns)
        return $ Right ()

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary (Method TestDB) where
    arbitrary = RunMigrationNumber <$> arbitrary

instance Arbitrary (Change TestDB) where
    arbitrary = Change <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

deriving instance Eq (Change TestDB)
deriving instance Show (Change TestDB)

newtype UniqueChanges = UniqueChanges [Change TestDB] deriving (Show)

instance Arbitrary UniqueChanges  where
  arbitrary = UniqueChanges <$> arbitrary `suchThat` uniqueNames
    where
      uniqueNames cs = let names = map changeName cs
                       in nub names == names

changeSequence :: [Change a] -> [Change a]
changeSequence [] = []
changeSequence (x:xs) = reverse $ snd $ foldl' go (x, []) xs
  where
    go :: (Change a, [Change a]) -> Change a -> (Change a, [Change a])
    go (lastChange, xs') c =
      let c' = c { changeDependencies = [changeName lastChange] }
      in (c', c':xs')
