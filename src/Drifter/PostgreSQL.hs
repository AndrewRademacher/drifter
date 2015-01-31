{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Drifter.PostgreSQL
    ( Postgres
    , Method (..)
    , DBConnection (..)
    ) where

import           Control.Applicative
import           Control.Exception
import           Data.ByteString                      (ByteString)
import           Data.Int
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types

import           Drifter.Types

data Postgres

data instance Method Postgres = Script ByteString
                              | Function (Connection -> IO (Either String ()))

data instance DBConnection Postgres = DBConnection Connection

newtype ChangeId = ChangeId { unChangeId :: Int } deriving (Eq, Ord, Show, FromField)

data ChangeHistory = ChangeHistory
        { histId          :: ChangeId
        , histName        :: Name
        , histDescription :: Maybe Description
        , histTime        :: UTCTime
        } deriving (Show)

instance Eq ChangeHistory where
    a == b = (histName a) == (histName b)

instance Ord ChangeHistory where
    compare a b = compare (histId a) (histId b)

instance FromRow ChangeHistory where
    fromRow = ChangeHistory <$> field <*> field <*> field <*> field

instance Drifter Postgres where
    migrate (DBConnection conn) changes = do
        _ <- bootstrap conn
        hist :: [(ChangeHistory)] <- query_ conn "SELECT id, name, description, time FROM drifter.changelog ORDER BY id"
        start <- findNext hist changes
        migratePostgres conn start

findNext :: [ChangeHistory] -> [Change Postgres] -> IO [Change Postgres]
findNext    []     cs                                   = return cs
findNext (h:hs) (c:cs) | (histName h) == (changeName c) = do putStrLn $ "Skipping: " ++ show (changeName c)
                                                             findNext hs cs
                       | otherwise                      = return (c:cs)
findNext     _      _                                   = do putStrLn "Change Set Exhausted"
                                                             return []

bootstrap :: Connection -> IO Int64
bootstrap conn = execute_ conn [sql|
BEGIN;

CREATE SCHEMA IF NOT EXISTS drifter;

CREATE TABLE IF NOT EXISTS drifter.changelog (
    id              serial      NOT NULL,
    name            text        NOT NULL,
    description     text,
    time            timestamp   NOT NULL,

    PRIMARY KEY (id),
    UNIQUE (name)
);

COMMIT;
|]

migratePostgres :: Connection -> [Change Postgres] -> IO (Either String ())
migratePostgres    _                [] = return $ Right ()
migratePostgres conn (Change n d m:cs) = do
        res <- handleChange conn m
        now <- getCurrentTime

        case res of
            Left  er -> return $ Left er
            Right  _ -> do _ <- logChange conn n d now
                           putStrLn $ "Committed: " ++ show n
                           migratePostgres conn cs

errorHandlers :: [Handler (Either String b)]
errorHandlers = [ Handler (\(ex::SqlError) -> return $ Left $ show ex)
                , Handler (\(ex::FormatError) -> return $ Left $ show ex)
                , Handler (\(ex::ResultError) -> return $ Left $ show ex)
                , Handler (\(ex::QueryError) -> return $ Left $ show ex)
                ]

handleChange :: Connection -> Method Postgres -> IO (Either String ())
handleChange conn (Script q) = (execute_ conn (Query q) >>= \_ -> return $ Right ())
    `catches` errorHandlers
handleChange conn (Function f) = (f conn >>= \_ -> return $ Right ())
    `catches` errorHandlers

logChange :: Connection -> Name -> Maybe Description -> UTCTime -> IO (Either String ())
logChange conn n d now = (execute conn insLog (n, d, now) >>= \_ -> return $ Right ())
    `catches` errorHandlers
    where insLog = [sql|
BEGIN;

INSERT INTO drifter.changelog (name, description, time)
    VALUES (?, ?, ?);

COMMIT;
|]
