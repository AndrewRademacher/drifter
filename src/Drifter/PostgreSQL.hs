{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Drifter.PostgreSQL
    ( Postgres
    , Method (..)
    , DBConnection (..)
    ) where

import           Data.ByteString
import           Data.Int
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           Drifter.Types

data Postgres = Postgres
        { conn :: Connection
        }

data instance Method Postgres = Script ByteString
                              | Function (Connection -> IO (Either String ()))

data instance DBConnection Postgres = DBConnection Connection

instance Drifter Postgres where
    migrate (DBConnection conn) changes = do
        _ <- bootstrap conn
        return $ Right ()
        {-migratePostgres conn changes-}

bootstrap :: Connection -> IO Int64
bootstrap conn = execute_ conn [sql|
BEGIN;

CREATE SCHEMA IF NOT EXISTS drifter;

CREATE TABLE IF NOT EXISTS drifter.changelog (
    id              serial                      NOT NULL,
    name            text                        NOT NULL,
    description     text,
    time            timestamp with time zone    NOT NULL,

    PRIMARY KEY (id),
    UNIQUE (name)
);

COMMIT;
|]

migratePostgres :: Connection -> [Change Postgres] -> IO (Either String ())
migratePostgres conn (c:cs) = undefined
