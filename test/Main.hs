{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative   as A
import           Data.IORef
import           Data.List
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Drifter
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "drifter"
    [
      graphTests
    , typesTests
    , changeSequenceTests
    ]

-------------------------------------------------------------------------------
changeSequenceTests :: TestTree
changeSequenceTests = testGroup "changeSequence"
  [
    testProperty "preserves list members" $ \((Blind cs) :: Blind [Change TestDB]) ->
      let cnames = changeName <$> cs
          cnames' = changeName <$> changeSequence cs
      in cnames === cnames'
  ]


-------------------------------------------------------------------------------
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
    c1 = Change c1n Nothing [] (RunMigrationNumber 1)
    c1n = ChangeName "c1"
    c2n = ChangeName "c2"
    c3n = ChangeName "c3"
    c2 = c1 { changeName = c2n, changeDependencies = [c1n], changeMethod = RunMigrationNumber 2}
    c3 = c1 { changeName = c3n, changeDependencies = [c2n], changeMethod = RunMigrationNumber 3}

data TestDB

newtype TestDBConn = TestDBConn (IORef [Int])

data instance Method TestDB = RunMigrationNumber Int deriving (Show, Eq)

data instance DBConnection TestDB = DBConnection TestDBConn

instance Drifter TestDB where
    migrateSingle (DBConnection (TestDBConn runs)) Change { changeMethod = RunMigrationNumber mn} = do
      modifyIORef runs (++ [mn])
      return $ Right ()

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

deriving instance Arbitrary ChangeName

instance Arbitrary (Method TestDB) where
    arbitrary = RunMigrationNumber <$> arbitrary

instance Arbitrary (Change TestDB) where
    arbitrary = Change <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

deriving instance Eq (Change TestDB)
deriving instance Show (Change TestDB)

newtype UniqueChanges = UniqueChanges [Change TestDB] deriving (Show)

instance Arbitrary UniqueChanges  where
  arbitrary = UniqueChanges A.<$> arbitrary `suchThat` uniqueNames
    where
      uniqueNames cs = let names = map changeName cs
                       in nub names == names
